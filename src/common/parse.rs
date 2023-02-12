#![allow(unused_imports)]
use miette::Diagnostic;
use nom::{
    branch::alt,
    character::complete::{multispace0, newline, space1},
    combinator::{all_consuming, eof, map, opt, value},
    multi::{fold_many1, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult, Parser,
};
use nom_locate::LocatedSpan;
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree, GenericErrorTree},
    final_parser::final_parser,
    multi::{collect_separated_terminated, parse_separated_terminated},
    parser_ext::ParserExt,
    tag::complete::tag,
};

use crate::common::piece::{Owner, Piece, PieceType};

type Span<'a> = LocatedSpan<&'a str>;
type ParseResult<'a, T> = IResult<Span<'a>, T, ErrorTree<Span<'a>>>;

fn parse_fen_positions(input: Span) -> ParseResult<Vec<Vec<Option<Piece>>>> {
    let mut parser = collect_separated_terminated(parse_rank_line, tag("/"), alt((space1, eof)));
    parser.parse(input)
}

fn parse_rank_line(input: Span) -> ParseResult<Vec<Option<Piece>>> {
    let empty_space_parser = map(nom::character::complete::u8, |count| {
        vec![None; count as usize]
    });
    let mut parser = fold_many1(
        alt((
            empty_space_parser,
            map(parse_piece, |piece| vec![Some(piece)]),
        )),
        || Vec::with_capacity(8),
        |mut acc: Vec<Option<Piece>>, pieces| {
            acc.extend(pieces);
            acc
        },
    );
    let res = parser(input);

    // TODO: Get nom to return a better error
    if let Ok((input, pieces)) = res {
        if pieces.len() == 8 {
            Ok((input, pieces))
        } else {
            panic!("Invalid rank line: {:?}", pieces);
        }
    } else {
        res
    }
}

fn parse_number_to_empty_spaces(input: Span) -> ParseResult<Vec<Option<Piece>>> {
    let (input, count) = nom::character::complete::u8(input)?;
    Ok((input, vec![None; count as usize]))
}

fn parse_piece(input: Span) -> ParseResult<Piece> {
    let mut parser = alt((
        tag("P").value(Piece::new(PieceType::Pawn, Owner::White)),
        tag("p").value(Piece::new(PieceType::Pawn, Owner::Black)),
        tag("R").value(Piece::new(PieceType::Rook, Owner::White)),
        tag("r").value(Piece::new(PieceType::Rook, Owner::Black)),
        tag("N").value(Piece::new(PieceType::Knight, Owner::White)),
        tag("n").value(Piece::new(PieceType::Knight, Owner::Black)),
        tag("B").value(Piece::new(PieceType::Bishop, Owner::White)),
        tag("b").value(Piece::new(PieceType::Bishop, Owner::Black)),
        tag("Q").value(Piece::new(PieceType::Queen, Owner::White)),
        tag("q").value(Piece::new(PieceType::Queen, Owner::Black)),
        tag("K").value(Piece::new(PieceType::King, Owner::White)),
        tag("k").value(Piece::new(PieceType::King, Owner::Black)),
    ));
    parser(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Board, Coord};
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_piece() {
        let input = "P".into();
        let (_, result) = all_consuming(parse_piece)(input).finish().unwrap();
        assert_eq!(result, Piece::new(PieceType::Pawn, Owner::White));

        let input = "p".into();
        let (_, result) = all_consuming(parse_piece)(input).finish().unwrap();
        assert_eq!(result, Piece::new(PieceType::Pawn, Owner::Black));
    }

    #[test]
    fn test_parse_empty_spaces() {
        let input = "4".into();
        let (_, result) = all_consuming(parse_number_to_empty_spaces)(input)
            .finish()
            .unwrap();
        assert_eq!(result, vec![None, None, None, None]);
    }

    #[test]
    fn test_parse_rank_line() {
        let input = "4P2p".into();
        let (_, result) = all_consuming(parse_rank_line)(input).finish().unwrap();
        assert_eq!(
            result,
            vec![
                None,
                None,
                None,
                None,
                Some(Piece::new(PieceType::Pawn, Owner::White)),
                None,
                None,
                Some(Piece::new(PieceType::Pawn, Owner::Black))
            ]
        );

        let input = "5N2".into();
        let (_, result) = all_consuming(parse_rank_line)(input).finish().unwrap();
        assert_eq!(
            result,
            vec![
                None,
                None,
                None,
                None,
                None,
                Some(Piece::new(PieceType::Knight, Owner::White)),
                None,
                None,
            ]
        );
    }

    #[test]
    fn test_parse_fen_positions() {
        let input = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R".into();
        let (_, result) = all_consuming(parse_fen_positions)(input).finish().unwrap();

        let board = Board::from_vec(result).expect("Unable to convert to board");
        let coord = Coord::from_string("a1").unwrap();
        let expected = Some(Piece::new(PieceType::Rook, Owner::White));
        assert_eq!(board.get(coord), expected, "White rook is a1");

        let board_ascii = board.as_ascii();
        let expected = indoc! {"
            rnbqkbnr
            pp.ppppp
            ........
            ..p.....
            ....P...
            .....N..
            PPPP.PPP
            RNBQKB.R
            "};

        assert_eq!(board_ascii, expected)
    }
}
