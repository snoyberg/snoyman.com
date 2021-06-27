use anyhow::*;
use chrono::prelude::*;
use once_cell::sync::OnceCell;
use reqwest::blocking::{Client, ClientBuilder};
use semver::Version;
use serde::Deserialize;
use std::collections::HashMap;
use std::env::args;
use std::fmt::{Display, Formatter};
use std::io::Write;

#[derive(Debug)]
struct Currency {
    rate: String,
    timestamp: DateTime<Utc>,
}

impl Currency {
    fn load() -> Result<Self> {
        let token = std::env::var("OPEN_EXCHANGE_RATE_TOKEN").context("Could not get OPEN_EXCHANGE_RATE_TOKEN")?;
        let url = format!("https://openexchangerates.org/api/latest.json?app_id={}", token);
        let mut res = get_client()
            .get(url)
            .send()?;
        let oer: Oer = serde_json::from_reader(&mut res)?;
        oer.into_currency()
    }
}

#[derive(Deserialize, Debug)]
struct Oer {
    timestamp: i64,
    rates: HashMap<String, f64>,
}

impl Oer {
    fn into_currency(self) -> Result<Currency> {
        let ils = self.rates.get("ILS").context("Israeli shekel not listed")?;
        let rate = format!("{:.3}", ils);
        Ok(Currency {
            rate,
            timestamp: Utc.timestamp(self.timestamp, 0),
        })
    }
}

impl Currency {
    fn write_shekel_md(&self, prefix: &str) -> Result<()> {
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
+++"#,
            date = self.timestamp.format("%B %e, %Y"),
            rate = self.rate,
        )?;
        Ok(())
    }

    fn write_shekel_feed(&self, prefix: &str) -> Result<()> {
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
      On {pretty}, $1 (United States Dollar) buys {rate}₪ (New Israeli Shekel).
    </content>
    <author>
      <name>Michael Snoyman</name>
    </author>
  </entry>
</feed>"#,
            updated = self.timestamp.to_rfc3339(),
            short = self.timestamp.format("%Y-%m-%d"),
            pretty = self.timestamp.format("%B %e, %Y"),
            rate = self.rate,
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

fn get_client() -> &'static Client {
    static CLIENT: OnceCell<Client> = OnceCell::new();
    CLIENT
        .get_or_try_init(|| ClientBuilder::new().use_rustls_tls().user_agent("snoycron").build())
        .unwrap()
}

struct AllGhcInfo(Vec<(Version, GhcInfo)>);

const GHC_INFO_URL: &str =
    "https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/global-hints.yaml";
fn load_ghc_info() -> Result<AllGhcInfo> {
    let mut res = get_client().get(GHC_INFO_URL).send()?;
    let m: HashMap<String, GhcInfo> = serde_yaml::from_reader(&mut res)?;
    let mut v: Vec<(Version, GhcInfo)> = Vec::new();
    for (name, info) in m {
        let prefix: String = name.chars().take(4).collect();
        ensure!(&prefix == "ghc-", "Invalid GHC version: {}", name);
        let version_str: String = name.chars().skip(4).collect();
        let version =
            Version::parse(&version_str).with_context(|| format!("Could not parse {}", name))?;
        v.push((version, info));
    }
    v.sort_by(|x, y| y.0.cmp(&x.0));
    Ok(AllGhcInfo(v))
}

impl Display for AllGhcInfo {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        writeln!(fmt, "+++")?;
        writeln!(fmt, "title = \"GHC/base/Cabal library versions\"")?;
        writeln!(fmt, "template = \"index.html\"")?;
        writeln!(fmt, "+++")?;
        writeln!(fmt, "This table correlates GHC versions with the versions of the base and Cabal libraries it ships with.")?;
        writeln!(fmt)?;
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
    let prefix = match args().nth(1) {
        None => return Err(anyhow!("Please provide root directory")),
        Some(x) => x,
    };

    let usd = Currency::load()?;
    usd.write_shekel_md(&prefix)?;
    usd.write_shekel_feed(&prefix)?;

    let ghc_info = load_ghc_info()?;
    let mut base_md = std::path::PathBuf::new();
    base_md.push(prefix);
    base_md.push("content");
    base_md.push("base.md");
    let mut base = std::fs::File::create(base_md)?;
    write!(base, "{}", ghc_info)?;

    Ok(())
}
