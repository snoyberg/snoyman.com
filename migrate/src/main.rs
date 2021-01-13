use serde::{Serialize, Deserialize};
use chrono::prelude::*;
use std::path::PathBuf;
use std::io::Write;
use anyhow::*;
use std::collections::{HashSet, HashMap};

#[derive(Serialize)]
struct Frontmatter<'a> {
    title: &'a str,
    description: Option<String>,
    path: String,
    date: toml::value::Datetime,
    taxonomies: HashMap<String, HashSet<String>>,
    extra: Extra,
}

#[derive(Serialize)]
struct Extra {
    ogimage: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Post {
    file: String,
    title: String,
    day: Option<chrono::NaiveDate>,
    time: Option<DateTime<Utc>>,
    listed: Option<bool>,
    description: Option<String>,
    #[serde(rename="twitter-image")]
    ogimage: Option<String>,
    #[serde(rename="old-slugs")]
    old_slugs: Option<std::collections::HashSet<String>>,
    series: Option<String>,
}

impl Post {
    fn frontmatter(&self, slug: &str) -> Result<Frontmatter> {
        let date = self.naive_date(slug)?;
        let mut taxonomies = HashMap::new();
        if let Some(ref series) = self.series {
            let mut set = HashSet::new();
            set.insert(series.clone());
            taxonomies.insert("series".to_owned(), set);
        }
        Ok(Frontmatter {
            title: &self.title,
            description: self.description.clone(),
            path: format!("/blog/{}/{:02}/{}", date.year(), date.month(), slug),
            date: date.to_string().parse()?,
            taxonomies,
            extra: Extra {
                ogimage: self.ogimage.clone(),
            }
        })
    }

    fn naive_date(&self, slug: &str) -> Result<NaiveDate> {
        match (self.time, self.day) {
            (None, None) => Err(anyhow!("No day or time in {}", slug)),
            (Some(time), None) => Ok(time.naive_local().date()),
            (None, Some(day)) => Ok(day),
            (Some(_), Some(_)) => Err(anyhow!("Both day and time in {}", slug)),
        }
    }

    fn convert(&self) -> anyhow::Result<()> {
        let mut input_path: PathBuf = PathBuf::new();
        input_path.push("..");
        input_path.push(&self.file);
        let mut output_path: PathBuf = PathBuf::new();
        output_path.push("..");
        output_path.push("content");
        output_path.push("blog");
        output_path.push(input_path.strip_prefix("../posts")?);
        if output_path.extension().map(|x| x.to_str()) != Some(Some("md")) {
            output_path.set_extension("md");
        }

        let mut input = std::fs::File::open(&input_path)?;
        let mut output = std::fs::File::create(&output_path)?;
        let slug: &str = output_path.file_stem().ok_or_else(|| anyhow!("No slug"))?.to_str().ok_or_else(|| anyhow!("Could not convert to str"))?;

        output.write_all(b"+++\n")?;
        let frontmatter = self.frontmatter(slug)?;
        let frontmatter: Vec<u8> = toml::to_vec(&frontmatter)?;
        output.write_all(&frontmatter)?;
        output.write_all(b"+++\n")?;
        std::io::copy(&mut input, &mut output)?;
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
struct Posts {
    posts: Vec<Post>,
}

fn main() -> anyhow::Result<()> {
    let file = std::fs::File::open("../posts.yaml")?;
    let posts: Posts = serde_yaml::from_reader(file)?;
    posts.posts.iter().map(Post::convert).collect()
}
