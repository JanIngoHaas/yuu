use miette::Result;
use yuu::pass_diagnostics::error;

fn main() -> Result<()> {
    // Setup error formatter with syntax highlighting (default theme)
    // error::setup_error_formatter(None, false)?;

    // Uncomment to use a different theme
    error::setup_error_formatter(Some("base16-mocha.dark"), true)?;

    // Your actual application code here
    println!("Hello, world!");

    Ok(())
}
