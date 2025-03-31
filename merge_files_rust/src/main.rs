use clap::Parser;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::ffi::OsStr;
use walkdir::WalkDir;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the merged output file (required)
    #[arg(short, long)]
    output: PathBuf,

    /// Directory containing .txt and .md files (defaults to current directory)
    #[arg(short, long, default_value = ".")]
    input: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let input_dir = args.input.canonicalize()
        .map_err(|e| format!("Failed to resolve input directory '{}': {}", args.input.display(), e))?;
    let output_file = args.output; // Keep output path as potentially relative

    println!("Input directory: {}", input_dir.display());
    println!("Output file: {}", output_file.display());

    if !input_dir.is_dir() {
        return Err(format!("Input path is not a directory: {}", input_dir.display()).into());
    }

    // --- Find and filter files ---
    let mut files_to_merge: Vec<PathBuf> = Vec::new();
    for entry in WalkDir::new(&input_dir).max_depth(1).min_depth(1) { // Only immediate children
        let entry = entry?;
        let path = entry.path();

        if path.is_file() {
            if let Some(ext) = path.extension().and_then(OsStr::to_str) {
                let lower_ext = ext.to_lowercase();
                if lower_ext == "txt" || lower_ext == "md" {
                    // Store the relative path from input_dir for display purposes
                    if let Ok(relative_path) = path.strip_prefix(&input_dir) {
                       files_to_merge.push(relative_path.to_path_buf());
                    } else {
                        // Fallback to absolute path if stripping fails (shouldn't happen here)
                        files_to_merge.push(path.to_path_buf());
                    }
                }
            }
        }
    }

    // Sort files alphabetically by their relative paths
    files_to_merge.sort();

    if files_to_merge.is_empty() {
        println!("No .txt or .md files found in directory: {}", input_dir.display());
        return Ok(());
    }

    println!("Found {} files to merge:", files_to_merge.len());
    for file in &files_to_merge {
        println!("  - {}", file.display());
    }

    // --- Create output directory if needed ---
     if let Some(parent_dir) = output_file.parent() {
        fs::create_dir_all(parent_dir)
            .map_err(|e| format!("Failed to create output directory '{}': {}", parent_dir.display(), e))?;
        println!("Ensured output directory exists: {}", parent_dir.display());
    }


    // --- Prepare output content ---
    let mut output_content = String::new();

    // Add Table of Contents
    output_content.push_str("<!-- Table of Contents -->\n");
    output_content.push_str("<!-- ===================== -->\n");
    for file_path in &files_to_merge {
        output_content.push_str(&format!("- {}\n", file_path.display()));
    }
    output_content.push_str("<!-- ===================== -->\n\n");


    // Add file contents with headers/footers
    for relative_path in &files_to_merge {
        let absolute_path = input_dir.join(relative_path);
        let filename = relative_path.file_name().unwrap_or_default().to_string_lossy(); // Get filename

        println!("Processing: {}", absolute_path.display());

        match fs::read_to_string(&absolute_path) {
             Ok(content) => {
                output_content.push_str(&format!("\n\n<!-- ========== Start: {} ========== -->\n\n", filename));
                output_content.push_str(content.trim());
                output_content.push_str(&format!("\n\n<!-- ========== End: {} ========== -->\n\n", filename));
            }
            Err(e) => {
                 eprintln!("\nWarning: Could not read file {}. Skipping. Error: {}", absolute_path.display(), e);
                 // Continue merging other files
             }
        }
    }

    // --- Write to output file ---
    let mut file = File::create(&output_file)
        .map_err(|e| format!("Failed to create output file '{}': {}", output_file.display(), e))?;

    file.write_all(output_content.trim().as_bytes())
         .map_err(|e| format!("Failed to write to output file '{}': {}", output_file.display(), e))?;

    println!("\nSuccessfully merged {} files into {}", files_to_merge.len(), output_file.display());

    Ok(())
}
