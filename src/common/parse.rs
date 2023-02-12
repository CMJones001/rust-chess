#![allow(unused_imports)]
use miette::{Diagnostic, GraphicalReportHandler};
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

#[derive(thiserror::Error, Debug, Diagnostic)]
#[error("Bad Input")]
struct BadInput {
    #[source_code]
    src: &'static str,

    #[label("{kind}")]
    bad_bit: miette::SourceSpan,

    kind: BaseErrorKind<&'static str, Box<dyn std::error::Error + Send + Sync>>,
}

fn get_error_message(error: ErrorTree<Span>, input: &'static str) {
    match error {
        GenericErrorTree::Base { location, kind } => {
            let offset = location.location_offset();
            let err = BadInput {
                src: input,
                bad_bit: miette::SourceSpan::new(offset.into(), 0.into()),
                kind,
            };
            let mut s = String::new();
            GraphicalReportHandler::new()
                .render_report(&mut s, &err)
                .unwrap();
            println!("{}", s);
        }
        GenericErrorTree::Stack { base, contexts } => {
            println!("Error log:");
            for (num, (span, context)) in contexts.into_iter().enumerate() {
                println!("  {num}. {context}: {span}");
            }
            get_error_message(*base, input);
        }
        GenericErrorTree::Alt(choices) => {
            println!("Expected one of the following:");
            for (num, choice) in choices.into_iter().enumerate() {
                println!("  Choice {}: ", num);
                get_error_message(choice, input);
            }
        }
    }
}

pub fn parse_fen_positions(input: Span) -> ParseResult<Vec<Vec<Option<Piece>>>> {
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
    parser(input)
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
    use crate::common::{Board, BoardError, Coord};
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

    /// The parse is valid, but missing a character, it cannot be converted into a board
    #[test]
    fn test_parse_fen_failed() {
        let input = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPP1PPP/RNBQKB1R".into();
        match parse_fen_positions(input).finish() {
            Ok((_, result)) => {
                let board = Board::from_vec(result);
                assert_eq!(
                    board.unwrap_err(),
                    BoardError::UnexpectedNumberOfFiles(7, 1)
                )
            }
            Err(e) => {
                get_error_message(e, &input);
                assert!(false, "Failed Parse")
            }
        }
    }
}
