use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Player {
    White,
    Black,
}

impl Player {
    pub fn opponent(&self) -> Player {
        if *self == Player::White {
            Player::Black
        } else {
            Player::White
        }
    }
}

impl Display for Player {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Player::White => write!(f, "White"),
            Player::Black => write!(f, "Black"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Piece {
    pub piece_type: PieceType,
    pub player: Player,
}

impl Display for Piece {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ascii())
    }
}

impl Piece {
    pub fn new(peice_type: PieceType, owner: Player) -> Piece {
        Piece {
            piece_type: peice_type,
            player: owner,
        }
    }

    pub fn as_ascii(&self) -> char {
        let letter = match self.piece_type {
            PieceType::Pawn => 'P',
            PieceType::Rook => 'R',
            PieceType::Knight => 'N',
            PieceType::Bishop => 'B',
            PieceType::Queen => 'Q',
            PieceType::King => 'K',
        };
        match self.player {
            Player::White => letter.to_uppercase().next().unwrap(),
            Player::Black => letter.to_lowercase().next().unwrap(),
        }
    }

    pub fn as_unicode(&self) -> char {
        match (self.piece_type, self.player) {
            (PieceType::Pawn, Player::White) => '♙',
            (PieceType::Pawn, Player::Black) => '♟',
            (PieceType::Rook, Player::White) => '♖',
            (PieceType::Rook, Player::Black) => '♜',
            (PieceType::Knight, Player::White) => '♘',
            (PieceType::Knight, Player::Black) => '♞',
            (PieceType::Bishop, Player::White) => '♗',
            (PieceType::Bishop, Player::Black) => '♝',
            (PieceType::Queen, Player::White) => '♕',
            (PieceType::Queen, Player::Black) => '♛',
            (PieceType::King, Player::White) => '♔',
            (PieceType::King, Player::Black) => '♚',
        }
    }
}
