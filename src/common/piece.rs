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
    pub peice_type: PieceType,
    pub owner: Owner,
}

impl Piece {
    pub fn new(peice_type: PieceType, owner: Owner) -> Piece {
        Piece { peice_type, owner }
    }

    pub fn as_ascii(&self) -> char {
        let letter = match self.peice_type {
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
}
