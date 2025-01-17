use std::str::Chars;

/// Peekable iterator over a char sequence.
///
/// Next characters can be peeked via `first` method,
/// and position can be shifted forward via `bump` method.
pub struct Cursor<'a> {
    line: usize,
    column: usize,
    /// Iterator over chars. Slightly faster than a &str.
    chars: Chars<'a>,
    #[cfg(debug_assertions)]
    prev: char,
}

pub(crate) const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            line: 1,
            column: 1,
            chars: input.chars(),
            #[cfg(debug_assertions)]
            prev: EOF_CHAR,
        }
    }

    fn inc_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn line(&self) -> usize {
        self.line
    }

    fn inc_column(&mut self) {
        self.column += 1;
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    /// Returns the last eaten symbol (or `'\0'` in release builds).
    /// (For debug assertions only.)
    pub(crate) fn prev(&self) -> char {
        #[cfg(debug_assertions)]
        {
            self.prev
        }

        #[cfg(not(debug_assertions))]
        {
            EOF_CHAR
        }
    }

    /// Peeks the next symbol from the input stream without consuming it.
    /// If requested position doesn't exist, `EOF_CHAR` is returned.
    /// However, getting `EOF_CHAR` doesn't always mean actual end of file,
    /// it should be checked with `is_eof` method.
    pub fn first(&self) -> char {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Peeks the third symbol from the input stream without consuming it.
    pub fn third(&self) -> char {
        // `.next()` optimizes better than `.nth(1)`
        let mut iter = self.chars.clone();
        iter.next();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Checks if there is nothing more to consume.
    pub(crate) fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Moves to the next character.
    pub(crate) fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        match c {
            '\n' => self.inc_line(),
            '\t' => self.column += 4,
            _ => self.inc_column(),
        }

        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }

        Some(c)
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    pub(crate) fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        // It was tried making optimized version of this for eg. line comments, but
        // LLVM can inline all of this and compile it down to fast iteration over bytes.
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }

    /// Eats symbols while predicate returns true or until the end of file is reached.
    pub(crate) fn eat_while_back(
        &mut self,
        buf: &mut String,
        mut predicate: impl FnMut(char) -> bool,
    ) {
        // It was tried making optimized version of this for eg. line comments, but
        // LLVM can inline all of this and compile it down to fast iteration over bytes.
        while predicate(self.first()) && !self.is_eof() {
            buf.push(self.bump().unwrap());
        }
    }
}
