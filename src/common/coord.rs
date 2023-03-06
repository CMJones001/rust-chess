use std::fmt::Display;

use super::errors::BoardError;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Coord {
    pub file: u8,
    pub rank: u8,
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let file = self.file + 97;
        let file = file as char;
        let rank = self.rank + 1;
        write!(f, "{}{}", file, rank)
    }
}

impl Coord {
    pub fn from_string(s: &str) -> Result<Coord, BoardError> {
        if s.len() != 2 {
            return Err(BoardError::MalformedCoordinateString(s.to_string()));
        }
        let mut chars = s.chars();
        let (file, rank) = (chars.next().unwrap(), chars.next().unwrap());
        let file = file as u8;
        let rank = rank as u8;
        if !(b'a'..=b'h').contains(&file) || !(b'1'..=b'8').contains(&rank) {
            return Err(BoardError::CoordinateOutOfRange((
                file as usize,
                rank as usize,
            )));
        }
        let file = file - 97;
        let rank = rank - 49;
        Ok(Coord { file, rank })
    }

    pub fn new(file: u8, rank: u8) -> Coord {
        if file > 7 || rank > 7 {
            panic!("Invalid coordinate: {}{}", file, rank);
        }
        Coord { file, rank }
    }

    pub fn from_index(index: usize) -> Coord {
        let file = (index % 8) as u8;
        let rank = (index / 8) as u8;
        Coord { file, rank }
    }

    /// Return the coordinate relative to the given value.
    /// If the coordinate is outside the board, return None.
    ///  dy is the change in rank, and dx is the change in file.
    ///  with dy = 1, dx = 0, we get the square above the current square,
    ///  toward the black pieces.
    pub fn relative(&self, dfile: i32, drank: i32) -> Option<Coord> {
        let file = self.file as i32 + dfile;
        let rank = self.rank as i32 + drank;
        if !(0..=7).contains(&file) || !(0..=7).contains(&rank) {
            None
        } else {
            Some(Coord {
                file: file as u8,
                rank: rank as u8,
            })
        }
    }
}
