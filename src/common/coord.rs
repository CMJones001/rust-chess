//! Coordinate representation and conversion.
//!
//! Coordinates are represented as a tuple of (file, rank), where file is the
//! column and rank is the row. The top left corner is (0, 0), and the bottom
//! right corner is (7, 7).
//!
//! Coordinates are converted to and from strings using the `Display` and
//! `FromStr` traits (or the `from_string` method). The string representation is the file letter
//! followed by the rank number, e.g. "a1", "b2", "c3", etc.
//!
//! Coordinates also have a `relative` method, which returns the coordinate
//! relative to itself, or `None` if this would be off the board.
//!
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

impl std::str::FromStr for Coord {
    type Err = BoardError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Coord::from_string(s)
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

    /// Convert an index in the board vector into a coordinate.
    pub fn from_index(index: usize) -> Coord {
        let file = (index % 8) as u8;
        let rank = (index / 8) as u8;
        Coord::new(file, rank)
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

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case(0, 0, "a1")]
    #[test_case(7, 7, "h8")]
    #[test_case(4, 2, "e3")]
    fn test_coord_to_string(file: u8, rank: u8, expected: &str) {
        let coord = Coord { file, rank };
        assert_eq!(coord.to_string(), expected);
    }

    #[test_case("a1", 0, 0)]
    #[test_case("h8", 7, 7)]
    #[test_case("e3", 4, 2)]
    fn test_coord_from_string(s: &str, expected_file: u8, expected_rank: u8) {
        let coord = Coord::from_string(s).unwrap();
        assert_eq!(coord.file, expected_file);
        assert_eq!(coord.rank, expected_rank);
    }

    /// Test that the `FromStr` trait works.
    #[test_case("a1", 0, 0)]
    #[test_case("h8", 7, 7)]
    #[test_case("e3", 4, 2)]
    fn test_coord_parse(s: &str, expected_file: u8, expected_rank: u8) {
        let coord: Coord = s.parse().unwrap();
        assert_eq!(coord.file, expected_file);
        assert_eq!(coord.rank, expected_rank);
    }

    #[test_case("a")]
    #[test_case("a1a")]
    fn test_coord_from_string_malformed(s: &str) {
        let coord = s.parse::<Coord>();
        assert!(matches!(
            coord.unwrap_err(),
            BoardError::MalformedCoordinateString(_)
        ));
    }

    #[test_case("a9")]
    #[test_case("i1")]
    fn test_coord_from_string_out_of_range(s: &str) {
        let coord = s.parse::<Coord>();
        assert!(matches!(
            coord.unwrap_err(),
            BoardError::CoordinateOutOfRange(_)
        ));
    }

    #[test_case("a1", 0, 0, "a1")]
    #[test_case("a1", 1, 0, "b1")]
    #[test_case("d1", 3, 2, "g3")]
    #[test_case("d5", -1, 0, "c5")]
    #[test_case("f1", 0, 7, "f8")]
    fn test_relative_coordinates(old_pos: &str, dfile: i32, drank: i32, expected: &str) {
        let old_pos: Coord = old_pos.parse().unwrap();
        let new_pos = old_pos.relative(dfile, drank).unwrap();
        assert_eq!(new_pos.to_string(), expected);
    }

    #[test_case("d1", 5, 0)]
    #[test_case("d1", 1, -1)]
    #[test_case("f1", 1, -1)]
    #[test_case("f1", 0, 8)]
    fn test_relative_coordinates_out_of_range(old_pos: &str, dfile: i32, drank: i32) {
        let old_pos: Coord = old_pos.parse().unwrap();
        let new_pos = old_pos.relative(dfile, drank);
        assert!(new_pos.is_none());
    }
}
