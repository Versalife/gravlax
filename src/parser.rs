//! A parser for the lox language implemented using the `parser-combinator pattern
//!
//!

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// A parser takes some input `I` and returns
/// the parsed output `O` along with the remaining
/// portion if the input `I`
#[derive(Debug, PartialEq)]
struct ParserOutput<I, O> {
    remainder: I,
    parsed_output: O,
}

impl<I, O> From<(I, O)> for ParserOutput<I, O> {
    fn from((i, o): (I, O)) -> Self {
        ParserOutput {
            remainder: i,
            parsed_output: o,
        }
    }
}

type ParserError<'a> = &'a str;
type ParseResult<'a, Input, Output> = Result<ParserOutput<Input, Output>, ParserError<'a>>;

trait Parser<'a, Input, Output> {
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output>;
}

// implement the `Parse` trait on all parsing functions
impl<'a, F, Input, Output> Parser<'a, Input, Output> for F
where
    F: Fn(Input) -> ParseResult<'a, Input, Output>,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output> {
        self(input)
    }
}

/// A parser that eats the letter `a`
fn the_letter_a(input: &str) -> Result<ParserOutput<&str, ()>, &str> {
    match input.chars().next() {
        Some('a') => Ok({
            let output = (&input['a'.len_utf8()..], ());
            ParserOutput::from(output)
        }),
        _ => Err(input),
    }
}

/// let’s write a function that produces a parser for a static string of any length, not just a single character.
///
/// This is very common in FP languages where every function takes only a single argument.
/// To simulate multiple arguments, we chain (using syntactic sugar of course) functions that
/// return other functions
///
/// ```haskell
/// el :: String -> String -> String
/// el tag content =
///     "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
/// ```
fn match_literal<'a>(expected: &'a str) -> impl Parser<'a, &str, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ()).into()),
        _ => Err(input),
    }
}

#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    assert_eq!(Ok(("", ()).into()), parse_joe.parse("Hello Joe!"));
    assert_eq!(
        Ok((" Hello Robert!", ()).into()),
        parse_joe.parse("Hello Joe! Hello Robert!")
    );
    assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
}

fn identifier(input: &str) -> ParseResult<&str, String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    // Verify that the first letter is alphanumeric
    match chars.next() {
        Some(next) if next.is_alphanumeric() => matched.push(next),
        _ => return Err(input),
    }

    // If we are here, then we have a valid identifier.
    // We now need to consume any other chars that are part
    // of this identifier
    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next)
        } else {
            break;
        }
    }
    Ok((&input[matched.len()..], matched).into())
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "i-am-an-identifier".to_string()).into()),
        identifier("i-am-an-identifier")
    );
    assert_eq!(
        Ok((" entirely an identifier", "not".to_string()).into()),
        identifier("not entirely an identifier")
    );
    assert_eq!(
        Err("!not at all an identifier"),
        identifier("!not at all an identifier")
    );
}

/// So now we can parse the opening <, and we can parse the following identifier,
/// but we need to parse both, in order, to be able to make progress here.
/// So the next step will be to write another parser builder function,
/// but one that takes two parsers as input and returns a new parser which parses both of them in order.
/// In other words, a parser combinator, because it combines two parsers into a new one
fn pair<'a, P1, P2, R1, R2>(first_parser: P1, second_parser: P2) -> impl Parser<'a, &'a str, (R1, R2)>
where
    P1: Parser<'a, &'a str, R1>,
    P2: Parser<'a, &'a str, R2>,
{
    //  we see that this is exactly what it does, too.
    // We start by running the first parser on the input, then the second parser,
    // then we combine the two results into a tuple and return that.
    // If either of those parsers fail, we return immediately with the error it gave.
    move |input| {
        first_parser.parse(input).and_then(|first_parser_output| {
            second_parser
                .parse(first_parser_output.remainder)
                .map(|second_parser_output| {
                    let combined_output = (first_parser_output.parsed_output, second_parser_output.parsed_output);
                    let remaining_input = second_parser_output.remainder;
                    ParserOutput::from((remaining_input, combined_output))
                })
        })
    }
}

#[test]
fn pair_combinator() {
    let tag_opener = pair(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", ((), "my-first-element".to_string())).into()),
        tag_opener.parse("<my-first-element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

/// we’re going to use our pair combinator to write two other
/// combinators: left, which discards the result of the first parser and only returns the second,
/// and its opposite number, right, which is the one we’d have wanted to use in our test above instead of pair -
/// the one that discards that () on the left hand side of the pair and only keeps our String.
///
///
/// A functor is a mapping between categories, It sends objects to objects and morphisms (functions)
/// to morphisms
///
/// This pattern is what’s called a “functor” in Haskell and its mathematical sibling, category theory.
/// If you’ve got a thing with a type A in it, and you have a map function available that you can pass a
/// function from A to B into to turn it into the same kind of thing but with the type B in it instead,
/// that’s a functor.
///
/// This function takes in a parser, applies it and, if it succeeds, applies
/// the provided `map_fn` to the parsed output
fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, &'a str, B>
where
    P: Parser<'a, &'a str, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|parser_out| (parser_out.remainder, map_fn(parser_out.parsed_output)).into())
    }
}

/// We use the pair combinator to combine the two parsers into a parser for a tuple of their results,
/// and then we use the map combinator to select just the result of the first
/// parse
fn project_left<'a, P1, P2, R1, R2>(first_parser: P1, second_parser: P2) -> impl Parser<'a, &'a str, R1>
where
    P1: Parser<'a, &'a str, R1>,
    P2: Parser<'a, &'a str, R2>,
{
    map(pair(first_parser, second_parser), |(left, _right)| left)
}

/// We use the pair combinator to combine the two parsers into a parser for a tuple of their results,
/// and then we use the map combinator to select just the result of the second
/// parser
fn project_right<'a, P1, P2, R1, R2>(first_parser: P1, second_parser: P2) -> impl Parser<'a, &'a str, R2>
where
    P1: Parser<'a, &'a str, R1>,
    P2: Parser<'a, &'a str, R2>,
{
    map(pair(first_parser, second_parser), |(_left, right)| right)
}

#[test]
fn right_combinator() {
    let tag_opener = project_right(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", "my-first-element".to_string()).into()),
        tag_opener.parse("<my-first-element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

/// if you wanted to get really fancy, you could write a combinator that takes a RangeBound
/// in addition to a parser and repeats it according to a range:
///     range(0..) for zero_or_more, range(1..) for one_or_more, range(5..=6) for exactly five or six,
///  wherever your heart takes you.
fn one_or_more<'a, A>(parser: impl Parser<'a, &'a str, A>) -> impl Parser<'a, &'a str, Vec<A>> {
    move |mut input| {
        // First, we parse the first element, and if it’s not there,
        // we return with an error. Then we parse as many more elements as we can,
        // until the parser fails, at which point we return the vector with the elements we collected.
        let mut result = Vec::new();
        if let Ok(first_output) = parser.parse(input) {
            result.push(first_output.parsed_output);
            input = first_output.remainder;
        } else {
            return Err(input);
        }

        while let Ok(cur_output) = parser.parse(input) {
            result.push(cur_output.parsed_output);
            input = cur_output.remainder;
        }

        Ok((input, result).into())
    }
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()]).into()), parser.parse("hahaha"));
    assert_eq!(Err("ahah"), parser.parse("ahah"));
    assert_eq!(Err(""), parser.parse(""));
}

/// Uses the provided parser to to generate a list with 0 or more values of the parsers
/// output
fn zero_or_more<'a, A>(parser: impl Parser<'a, &'a str, A>) -> impl Parser<'a, &'a str, Vec<A>> {
    move |mut input| {
        // This is an adaptation of the `one_or_more` parser builder to the case of
        // zero or more.
        let mut result = Vec::new();
        while let Ok(cur_output) = parser.parse(input) {
            result.push(cur_output.parsed_output);
            input = cur_output.remainder;
        }

        Ok((input, result).into())
    }
}

/// Note the difference between the two:
/// for one_or_more, finding an empty string is an error,
/// because it needs to see at least one case of its sub-parser,
///  but for zero_or_more, an empty string just means the zero case,
///  which is not an error.
#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()]).into()), parser.parse("hahaha"));
    assert_eq!(Ok(("ahah", vec![]).into()), parser.parse("ahah"));
    assert_eq!(Ok(("", vec![]).into()), parser.parse(""));
}

fn any_char(input: &str) -> ParseResult<&str, char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next).into()),
        _ => Err(input),
    }
}

fn predicate<'a, O, P, F>(parser: P, predicate: F) -> impl Parser<'a, &'a str, O>
where
    P: Parser<'a, &'a str, O>,
    F: Fn(&O) -> bool,
{
    move |input| match parser.parse(input) {
        Err(e) => Err(e),
        Ok(parser_output) => {
            if predicate(&parser_output.parsed_output) {
                return Ok(parser_output);
            }
            Err(input)
        }
    }
}

#[test]
fn predicate_combinator() {
    // Create a parser that parses the character 'o'
    let parser = predicate(any_char, |c| *c == 'o');

    // The parser should consume the 'o' at the start and return 'mg'
    assert_eq!(Ok(("mg", 'o').into()), parser.parse("omg"));

    // The parser should fail
    assert_eq!(Err("lol"), parser.parse("lol"));
}

fn eat_single_whitespace<'a>() -> impl Parser<'a, &'a str, char> {
    predicate(any_char, |c| c.is_whitespace())
}

fn one_or_more_whitespaces<'a>() -> impl Parser<'a, &'a str, Vec<char>> {
    one_or_more(eat_single_whitespace())
}

fn zero_or_more_whitespaces<'a>() -> impl Parser<'a, &'a str, Vec<char>> {
    zero_or_more(eat_single_whitespace())
}

fn eat_quoted_string<'a>() -> impl Parser<'a, &'a str, String> {
    let eat_opening_quote = match_literal("\"");
    let eat_closing_quote = match_literal("\"");
    let eat_string_chars = zero_or_more(predicate(any_char, |&c| c != '"'));

    map(
        project_right(eat_opening_quote, project_left(eat_string_chars, eat_closing_quote)),
        |chars| chars.iter().collect(),
    )
}

#[test]
fn quoted_string_parser() {
    let parser = eat_quoted_string();
    assert_eq!(
        Ok(("", "Hello There".to_string()).into()),
        parser.parse("\"Hello There\"")
    )
}

type AttributePair = (String, String);

/// Create a parser that parses strings orf the form, `attribute_name="attribute_value"`
fn eat_attribute_pair<'a>() -> impl Parser<'a, &'a str, AttributePair> {
    pair(identifier, project_right(match_literal("="), eat_quoted_string()))
}

type AttributePairs = Vec<AttributePair>;
/// Zero or more occurrences of the following: one or more whitespace characters, then an attribute pair.
/// We use right to discard the whitespace and keep the attribute pair.
fn eat_all_attribute_pairs<'a>() -> impl Parser<'a, &'a str, AttributePairs> {
    let eat_space_then_attribute = project_right(one_or_more_whitespaces(), eat_attribute_pair());
    zero_or_more(eat_space_then_attribute)
}

#[test]
fn attribute_parser() {
    assert_eq!(
        Ok((
            "",
            vec![
                ("one".to_string(), "1".to_string()),
                ("two".to_string(), "2".to_string())
            ]
        )
            .into()),
        eat_all_attribute_pairs().parse(" one=\"1\" two=\"2\"")
    );
}

type XmlElementStart = (String, AttributePairs);
fn eat_element_start<'a>() -> impl Parser<'a, &'a str, XmlElementStart> {
    let eat_opening_angle_bracket = match_literal("<");
    let eat_identifier_then_all_attributes = pair(identifier, eat_all_attribute_pairs());
    project_right(eat_opening_angle_bracket, eat_identifier_then_all_attributes)
}

fn parse_single_element<'a>() -> impl Parser<'a, &'a str, Element> {
    let start = project_left(eat_element_start(), match_literal("/>"));
    let element_constructor = |(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    };
    map(start, element_constructor)
}


#[test]
fn single_element_parser() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "div".to_string(),
                attributes: vec![("class".to_string(), "float".to_string())],
                children: vec![]
            }
        ).into()),
        parse_single_element().parse("<div class=\"float\"/>")
    );
}