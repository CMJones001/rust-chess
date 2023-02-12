#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Owner {
    White,
    Black,
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
    pub owner: Owner,
}

impl Piece {
    pub fn new(peice_type: PieceType, owner: Owner) -> Piece {
        Piece {
            piece_type: peice_type,
            owner,
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
        match self.owner {
            Owner::White => letter.to_uppercase().next().unwrap(),
            Owner::Black => letter.to_lowercase().next().unwrap(),
        }
    }

    pub fn as_unicode(&self) -> char {
        match (self.piece_type, self.owner) {
            (PieceType::Pawn, Owner::White) => '♙',
            (PieceType::Pawn, Owner::Black) => '♟',
            (PieceType::Rook, Owner::White) => '♖',
            (PieceType::Rook, Owner::Black) => '♜',
            (PieceType::Knight, Owner::White) => '♘',
            (PieceType::Knight, Owner::Black) => '♞',
            (PieceType::Bishop, Owner::White) => '♗',
            (PieceType::Bishop, Owner::Black) => '♝',
            (PieceType::Queen, Owner::White) => '♕',
            (PieceType::Queen, Owner::Black) => '♛',
            (PieceType::King, Owner::White) => '♔',
            (PieceType::King, Owner::Black) => '♚',
        }
    }
}
