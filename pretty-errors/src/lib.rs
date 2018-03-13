#[cfg(feature = "color")]
extern crate ansi_term;
#[cfg(feature = "color")]
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
mod tests;

use std::borrow::Cow;
use std::cmp::max;
use std::fmt::{Display, Formatter, Result as FmtResult, Write};
use std::ops::Range;

#[cfg(feature = "color")]
use ansi_term::{Colour, Style};

/// A helper struct for rendering a message attached to a span of source code.
///
/// Operations on this struct may panic if `start` or `end` are outside the
/// range of `src`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Message<'a> {
    #[cfg(feature = "color")] pub color: bool,
    pub path: Option<&'a str>,
    pub text: Cow<'a, str>,

    barlen: usize,
    start: (usize, usize),
    end: (usize, usize),
    src: &'a str,
}

impl<'a> Message<'a> {
    fn bar(&self, fmt: &mut Formatter, n: Option<usize>) -> FmtResult {
        self.start_bar(fmt)?;
        write!(fmt, " ")?;
        if let Some(n) = n {
            write!(fmt, "{0:>1$}", n, self.barlen)?;
        } else {
            Message::spaces(fmt, self.barlen)?;
        }
        write!(fmt, " | ")?;
        self.end_bar(fmt)
    }

    fn barln(&self, fmt: &mut Formatter) -> FmtResult {
        self.bar(fmt, None)?;
        writeln!(fmt)
    }

    fn line(&self, n: usize) -> &'a str {
        assert_ne!(n, 0);
        self.src.lines().skip(n - 1).next().unwrap_or("")
    }

    fn line_range(&self) -> Range<usize> {
        assert!(self.start.0 <= self.end.0);
        let mut start = self.start.0;
        let mut end = self.end.0 + 1;
        if start > 1 {
            start -= 1;
        }
        if end < self.src.lines().count() {
            end += 1;
        }
        start..end
    }

    fn multiple(fmt: &mut Formatter, ch: char, n: usize) -> FmtResult {
        for _ in 0..n {
            fmt.write_char(ch)?;
        }
        Ok(())
    }

    /// Creates a new Message. `start` and `end` *MUST* be within bounds, or
    /// panics will abound.
    pub fn new(
        start: (usize, usize),
        end: (usize, usize),
        text: Cow<'a, str>,
        path: Option<&'a str>,
        src: &'a str,
    ) -> Message<'a> {
        let barlen = ((max(start.0, end.0) + 2) as f32).log10().ceil();
        Message {
            #[cfg(feature = "color")]
            color: false,
            barlen: barlen as usize,
            start,
            end,
            path,
            text,
            src,
        }
    }

    fn spaces(fmt: &mut Formatter, n: usize) -> FmtResult {
        Message::multiple(fmt, ' ', n)
    }

    /// Enables color.
    #[cfg(feature = "color")]
    pub fn with_color(&mut self) -> &mut Self {
        self.color = true;
        self
    }
}

#[cfg(feature = "color")]
lazy_static!{
    static ref BAR_STYLE: Style = Style::new().bold().fg(Colour::Blue);
    static ref ERROR_STYLE: Style = Style::new().bold().fg(Colour::Red);
}

#[cfg(feature = "color")]
impl<'a> Message<'a> {
    fn start_bar(&self, fmt: &mut Formatter) -> FmtResult {
        if self.color {
            BAR_STYLE.prefix().fmt(fmt)
        } else {
            Ok(())
        }
    }

    fn end_bar(&self, fmt: &mut Formatter) -> FmtResult {
        if self.color {
            BAR_STYLE.suffix().fmt(fmt)
        } else {
            Ok(())
        }
    }

    fn start_error(&self, fmt: &mut Formatter) -> FmtResult {
        if self.color {
            ERROR_STYLE.prefix().fmt(fmt)
        } else {
            Ok(())
        }
    }

    fn end_error(&self, fmt: &mut Formatter) -> FmtResult {
        if self.color {
            ERROR_STYLE.suffix().fmt(fmt)
        } else {
            Ok(())
        }
    }
}

#[cfg(not(feature = "color"))]
impl<'a> Message<'a> {
    fn start_bar(&self, fmt: &mut Formatter) -> FmtResult {
        Ok(())
    }

    fn end_bar(&self, fmt: &mut Formatter) -> FmtResult {
        Ok(())
    }

    fn start_error(&self, fmt: &mut Formatter) -> FmtResult {
        Ok(())
    }

    fn end_error(&self, fmt: &mut Formatter) -> FmtResult {
        Ok(())
    }
}

impl<'a> Display for Message<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        // Draw the arrow for the file name, if present.
        if let Some(path) = self.path {
            Message::spaces(fmt, self.barlen + 1)?;
            self.start_bar(fmt)?;
            write!(fmt, "-->")?;
            self.end_bar(fmt)?;
            writeln!(fmt, " {}:{}:{}", path, self.start.0, self.start.1)?;
        }

        self.barln(fmt)?;
        for l in self.line_range() {
            self.bar(fmt, Some(l))?;
            let line = self.line(l);
            writeln!(fmt, "{}", line)?;
            if l == self.start.0 {
                self.bar(fmt, None)?;
                Message::spaces(fmt, self.start.1)?;
                self.start_error(fmt)?;
                if self.start.0 == self.end.0 {
                    let l = self.end.1 - self.start.1;
                    Message::multiple(
                        fmt,
                        '^',
                        if l == 0 { 1 } else { l - 1 },
                    )?;
                    writeln!(fmt, "^ {}", self.text)?;
                } else {
                    writeln!(fmt, "^")?;
                    // TODO
                }
                self.end_error(fmt)?;
            }
        }
        self.bar(fmt, None)
    }
}
