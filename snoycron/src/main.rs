use anyhow::*;
use chrono::prelude::*;
use reqwest::blocking::get;
use semver::Version;
use serde::Deserialize;
use std::collections::HashMap;
use std::env::args;
use std::fmt::{Display, Formatter};
use std::io::Write;

#[derive(Deserialize, Debug)]
struct Currencies {
    #[serde(rename = "LAST_UPDATE")]
    last_update: NaiveDate,
    #[serde(rename = "CURRENCY")]
    currencies: Vec<Currency>,
}

#[derive(Deserialize, Debug)]
struct Currency {
    #[serde(rename = "CURRENCYCODE")]
    code: String,
    #[serde(rename = "RATE")]
    rate: String,
    #[serde(rename = "CHANGE")]
    change: String,
}

impl Currencies {
    fn find(&self, code: &str) -> Result<&Currency> {
        self.currencies
            .iter()
            .find(|x| x.code == code)
            .ok_or_else(|| anyhow!("Currency code not found: {}", code))
    }

    fn load() -> Result<Currencies> {
        Ok(serde_xml_rs::from_reader(get("https://www.boi.org.il/currency.xml")?)?)
    }
}

impl Currency {
    fn write_shekel_md(&self, prefix: &str, last_update: NaiveDate) -> Result<()> {
        let mut file = std::path::PathBuf::new();
        file.push(prefix);
        file.push("content");
        file.push("shekel.md");
        let mut file = std::fs::File::create(&file)?;
        writeln!(
            file,
            r#"+++
title = "Dollar versus Shekel"
template = "shekel.html"
[extra]
date = "{date}"
rate = "{rate}"
delta = "{delta}"
+++"#,
            date = last_update.format("%B %e, %Y"),
            rate = self.rate,
            delta = self.delta(),
        )?;
        Ok(())
    }

    fn delta(&self) -> String {
        match self.change.chars().next() {
            Some('-') => format!(
                "{}% weaker",
                self.change.chars().skip(1).collect::<String>()
            ),
            _ => format!("{}% stronger", self.change),
        }
    }

    fn write_shekel_feed(&self, prefix: &str, last_update: NaiveDate) -> Result<()> {
        let last_update_time: DateTime<Utc> = DateTime::from_utc(last_update.and_hms(0, 0, 0), Utc);
        let mut file = std::path::PathBuf::new();
        file.push(prefix);
        file.push("static");
        file.push("shekel");
        file.push("feed.xml");
        let mut file = std::fs::File::create(&file)?;
        writeln!(
            file,
            r#"<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>Dollar versus Shekel</title>
  <link href="https://www.snoyman.com/shekel/feed.xml" rel="self"/>
  <link href="https://www.snoyman.com/shekel/"/>
  <updated>{updated}</updated>
  <id>https://www.snoyman.com/shekel</id>
  <entry>
    <id>https://www.snoyman.com/shekel?{short}</id>
    <link href="https://www.snoyman.com/shekel?{updated}"/>
    <updated>{updated}</updated>
    <title>Dollar versus Shekel: {pretty}</title>
    <content type="html">
      On {pretty}, $1 (United States Dollar) buys {rate}₪ (New Israeli Shekel). The dollar became {delta}.
    </content>
    <author>
      <name>Michael Snoyman</name>
    </author>
  </entry>
</feed>"#,
            updated = last_update_time.to_rfc3339(),
            short = last_update.format("%Y-%m-%d"),
            pretty = last_update.format("%B %e, %Y"),
            rate = self.rate,
            delta = self.delta(),
        )?;
        Ok(())
    }
}

#[derive(Deserialize, Debug)]
struct GhcInfo {
    base: String,
    #[serde(rename = "Cabal")]
    cabal: String,
    #[serde(rename = "Win32")]
    win32: String,
}

struct AllGhcInfo(Vec<(Version, GhcInfo)>);

const GHC_INFO_URL: &str =
    "https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/global-hints.yaml";
fn load_ghc_info() -> Result<AllGhcInfo> {
    let res = get(GHC_INFO_URL)?;
    let m: HashMap<String, GhcInfo> = serde_yaml::from_reader(res)?;
    let v: Vec<(Version, GhcInfo)> = Vec::with_capacity(m.len());
    m.into_iter().map(|(name, info)| {
        let prefix: String = name.chars().take(4).collect();
        ensure!(&prefix == "ghc-", "Invalid GHC version: {}", name);
        let version_str: String = name.chars().skip(4).collect();
        let version =
            Version::parse(&version_str).with_context(|| format!("Could not parse {}", name))?;
        Ok((version, info))
    }).collect::<Result<_>>().map(AllGhcInfo)
}

impl Display for AllGhcInfo {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        writeln!(fmt, "+++")?;
        writeln!(fmt, "title = \"GHC/base/Cabal library versions\"")?;
        writeln!(fmt, "template = \"index.html\"")?;
        writeln!(fmt, "+++")?;
        writeln!(fmt, "This table correlates GHC versions with the versions of the base and Cabal libraries it ships with.")?;
        writeln!(fmt, "")?;
        writeln!(fmt, "<table id=\"versions\"><thead><tr><th>GHC</th><th>base</th><th>Cabal</th><th>Win32</th></tr></thead><tbody>")?;
        for (ghc, info) in &self.0 {
            writeln!(
                fmt,
                "<tr><td>ghc-{}</td><td>base-{}</td><td>Cabal-{}</td><td>Win32-{}</td></tr>",
                ghc, info.base, info.cabal, info.win32
            )?;
        }
        writeln!(fmt, "</tbody></table>")
    }
}

fn main() -> Result<()> {
    let prefix = match args().skip(1).next() {
        None => return Err(anyhow!("Please provide root directory")),
        Some(x) => x,
    };

    let currencies: Currencies = Currencies::load()?;
    let usd = currencies.find("USD")?;
    usd.write_shekel_md(&prefix, currencies.last_update)?;
    usd.write_shekel_feed(&prefix, currencies.last_update)?;

    let ghc_info = load_ghc_info()?;
    let mut base_md = std::path::PathBuf::new();
    base_md.push(prefix);
    base_md.push("content");
    base_md.push("base.md");
    let mut base = std::fs::File::create(base_md)?;
    write!(base, "{}", ghc_info)?;

    Ok(())
}
