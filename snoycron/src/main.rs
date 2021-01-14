use serde::{Serialize, Deserialize};
use reqwest::blocking::get;
use std::collections::HashMap;
use semver::Version;
use anyhow::*;
use std::env::args;
use std::fmt::{Display, Formatter};
use std::io::Write;

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
    let mut res = get(GHC_INFO_URL)?;
    let m: HashMap<String, GhcInfo> = serde_yaml::from_reader(&mut res)?;
    let mut v: Vec<(Version, GhcInfo)> = Vec::new();
    for (name, info) in m {
        let prefix: String = name.chars().take(4).collect();
        ensure!(&prefix == "ghc-", "Invalid GHC version: {}", name);
        let version_str: String = name.chars().skip(4).collect();
        let version = Version::parse(&version_str).with_context(|| format!("Could not parse {}", name))?;
        v.push((version, info));
    }
    v.sort_by(|x, y| y.0.cmp(&x.0));
    Ok(AllGhcInfo(v))
}

impl Display for AllGhcInfo {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        writeln!(fmt, "+++")?;
        writeln!(fmt, "title = \"GHC/base/Cabal library versions\"")?;
        writeln!(fmt, "template = \"index.html\"");
        writeln!(fmt, "+++")?;
        writeln!(fmt, "This table correlates GHC versions with the versions of the base and Cabal libraries it ships with.")?;
        writeln!(fmt, "")?;
        writeln!(fmt, "<table id=\"versions\"><thead><tr><th>GHC</th><th>base</th><th>Cabal</th><th>Win32</th></tr></thead><tbody>")?;
        for (ghc, info) in &self.0 {
            writeln!(fmt, "<tr><td>ghc-{}</td><td>base-{}</td><td>Cabal-{}</td><td>Win32-{}</td></tr>", ghc, info.base, info.cabal, info.win32)?;
        }
        writeln!(fmt, "</tbody></table>")
    }
}

fn main() -> Result<()> {
    let prefix = match args().skip(1).next() {
        None => return Err(anyhow!("Please provide root directory")),
        Some(x) => x,
    };

    let ghc_info = load_ghc_info()?;
    let mut base_md = std::path::PathBuf::new();
    base_md.push(prefix);
    base_md.push("content");
    base_md.push("base.md");
    let mut base = std::fs::File::create(base_md)?;
    write!(base, "{}", ghc_info)?;

    Ok(())
}
