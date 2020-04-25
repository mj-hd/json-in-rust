/**
 * json := element
 *
 * value := object | array | string | number | true | false | null
 * First(value) = '{' , '[' , '"' , '0'...'9' , 't' , 'f' , 'n'
 * Follow(value) = ',', ws
 *
 * true := 't', 'r', 'u', 'e'
 * First(true) = 't'
 * Follow(true) = ',', ws
 *
 * false := 'f', 'a', 'l', 's', 'e'
 * First(false) = 'f'
 * Follow(false) = ',', ws
 *
 * null := 'n', 'u', 'l', 'l'
 * First(null) = 'n'
 * Follow(null) = ',', ws
 *
 * object := '{', ws, [ members ], ws, '}'
 * First(object) = '{'
 * Follow(object) = ',', ws
 *
 * ws := { \0020 | \000A | \000D | \0009 }
 *
 * members := member, { ',', members }
 * First(members) = e, ws, character
 * Follow(members) = ws, '}'
 *
 * member := ws, string, ws, ':', element
 * First(member) = e, ws, character
 * Follow(member) = ws, ',', '}'
 *
 * array := '[', ws, elements, ws, ']'
 * First(array) = '['
 * Follow(array) = ',', ws
 *
 * elements := element, [ ',', elements ]
 * First(elements) = e, ws
 * Follow(elements) = ws, ']', EOF
 *
 * element := ws, value, ws
 * First(element) = e, ws
 * Follow(element) = ',', ws, '}', ']', EOF
 *
 * string := '"', { character }, '"'
 * First(string) = '"'
 * Follow(string) = ',', ws, ':'
 *
 * character := (\0020 ... \10ffff - '"' - '\') | '\', escape
 *
 * escape := '"' | '\' | '/' | 'b' | 'n' | 'r' | 't' | 'u', hex, hex, hex, hex
 *
 * hex := digit | 'A' ... 'F' | 'a' ... 'f'
 *
 * number := integer, fraction, exponent
 * First(number) = '0'-'9', '-'
 * Follow(number) = ',', ws
 *
 * integer := digit | onenine, digits | '-', digit | '-', onenine, digits
 * First(integer) = '0'-'9', '-'
 * Follow(integer) = '.', 'e', 'E', ',', ws
 *
 * digits := digit | digit, digits
 *
 * fraction := [ '.', digits ]
 * First(fraction) = e, '.'
 * Follow(fraction) = 'E', 'e', ',', ws
 *
 * exponent := [ 'E', sign, digits | 'e', sign, digits ]
 * First(exponent) = e, 'E', 'e'
 * Follow(exponent) = ',', ws
 *
 * sign := [ '+' | '-' ]
 *
 * digit := '0'..'9'
 * onenine := '1'...'9'
 *
 */

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Token {
    JsonStart,
    JsonEnd,
    Bool(bool),
    Null,
    ObjectStart,
    ObjectEnd,
    MemberStart,
    MemberEnd,
    ArrayStart,
    ArrayEnd,
    ValueStart,
    ValueEnd,
    String(String),
    NumberStart,
    NumberEnd,
    Integer(i64),
    Fraction(i64),
    Exponent(i64),
}

struct Context<'s> {
    src: &'s str,
    i: usize,
}

impl<'s> Context<'s> {
    pub fn new(src: &'s str) -> Context {
        Context { src, i: 0 }
    }

    pub fn peek(&self) -> Option<char> {
        self.src.chars().nth(self.i)
    }

    pub fn next(&mut self) -> Option<char> {
        let val = (*self).peek();
        self.i += 1;

        val
    }

    pub fn take(&mut self, n: usize) -> Option<String> {
        if self.src.len() <= self.i + n {
            return None;
        }
        let val = Some(self.src[self.i..self.i + n].to_string());

        self.i += n;

        val
    }

    pub fn debug(&self) -> String {
        match self.peek() {
            Some(x) => format!("src: {}, char: {}", self.src, x),
            _ => "empty".to_string(),
        }
    }
}

struct Lexer {}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {}
    }

    pub fn tokenize(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        Ok(vec![self.json_start(ctx)?, self.json_end(ctx)?].into_iter().flatten().collect::<Vec<Token>>())
    }

    fn json_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        Ok(vec![
            vec![Token::JsonStart],
            self.element_start(ctx)?,
        ]
        .into_iter().flatten().collect::<Vec<Token>>())
    }

    fn json_end(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        match ctx.peek() {
            None => Ok(vec![Token::JsonEnd]),
            _ => Err("invalid json".to_string()),
        }
    }

    fn element_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        self.ws_start(ctx)?;

        let tokens = self.value(ctx)?;

        self.ws_start(ctx)?;

        Ok(tokens)
    }

    fn value(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        Ok(vec![
            vec![Token::ValueStart],
            (match ctx.peek() {
                Some('{') => self.object_start(ctx),
                Some('[') => self.array_start(ctx),
                Some('"') => self.string(ctx),
                Some(('0'..='9')) => self.number(ctx),
                Some('t') => self.json_true(ctx),
                Some('f') => self.json_false(ctx),
                Some('n') => self.null(ctx),
                Some(_) => Err("invalid value".to_string()),
                None => Ok(vec![]),
            })?,
            vec![Token::ValueEnd],
        ].into_iter().flatten().collect::<Vec<Token>>())
    }

    fn json_true(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.take(4) != Some("true".to_string()) {
            return Err("invalid boolean".to_string());
        }

        match ctx.peek() {
            ch if self.is_follow_value(ch) => Ok(vec![Token::Bool(true)]),
            _ => Err("invalid bool".to_string()),
        }
    }

    fn json_false(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.take(5) != Some("false".to_string()) {
            return Err("invalid boolean".to_string());
        }

        match ctx.peek() {
            ch if self.is_follow_value(ch) => Ok(vec![Token::Bool(false)]),
            _ => Err("invalid bool".to_string()),
        }
    }

    fn null(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.take(4) != Some("null".to_string()) {
            return Err("invalid null".to_string());
        }

        match ctx.peek() {
            ch if self.is_follow_value(ch) => Ok(vec![Token::Null]),
            _ => Err("invalid bool".to_string()),
        }
    }

    fn object_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.next() != Some('{') {
            return Err("invalid object".to_string());
        }

        self.ws_start(ctx)?;

        let tokens = self.members_start(ctx)?;

        self.ws_start(ctx)?;

        if ctx.next() != Some('}') {
            return Err("invalid object".to_string());
        }

        Ok(vec![vec![Token::ObjectStart], tokens, self.object_end(ctx)?].into_iter().flatten().collect::<Vec<Token>>())
    }

    fn is_follow_value(&self, ch: Option<char>) -> bool {
        match ch {
            Some(',') => true,
            None => true,
            ch if self.is_ws(ch) => true,
            _ => false,
        }
    }

    fn object_end(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        match ctx.peek() {
            ch if self.is_follow_value(ch) => Ok(vec![Token::ObjectEnd]),
            _ => Err("invalid object".to_string()),
        }
    }

    fn is_ws(&self, ch: Option<char>) -> bool {
        match ch {
             Some('\u{0020}') | Some('\u{000a}') | Some('\u{000d}') | Some('\u{0009}') => true,
             _ => false,
        }
    }

    fn ws_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        while self.is_ws(ctx.peek()) {
            ctx.next();
        }
        Ok(vec![])
    }

    fn members_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        let member = self.member_start(ctx)?;

        let mut tokens = Vec::from(member);

        if ctx.peek() == Some(',') {
            ctx.next();

            tokens.append(&mut self.members_start(ctx)?);
        }

        Ok(tokens)
    }

    fn member_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        self.ws_start(ctx)?;

        let string = self.string(ctx)?;

        self.ws_start(ctx)?;

        if ctx.next() != Some(':') {
            return Err("invalid member".to_string());
        }

        let element = self.element_start(ctx)?;

        Ok(vec![vec![Token::MemberStart], string, element, vec![Token::MemberEnd]].into_iter().flatten().collect::<Vec<Token>>())
    }

    fn array_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.next() != Some('[') {
            return Err("invalid array".to_string());
        }

        self.ws_start(ctx)?;

        let tokens = self.elements_start(ctx)?;

        self.ws_start(ctx)?;

        if ctx.next() != Some(']') {
            return Err("invalid array".to_string());
        }

        Ok(vec![vec![Token::ArrayStart], tokens, self.array_end(ctx)?].into_iter().flatten().collect::<Vec<Token>>())
    }


    fn array_end(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        match ctx.peek() {
            ch if self.is_follow_value(ch) => Ok(vec![Token::ArrayEnd]),
            _ => Err("invalid array".to_string()),
        }
    }

    fn elements_start(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        let element = self.element_start(ctx)?;

        let mut tokens = Vec::from(element);

        if ctx.peek() == Some(',') {
            ctx.next();

            tokens.append(&mut self.elements_start(ctx)?);
        }

        Ok(tokens)
    }

    fn is_follow_string(&self, ch: Option<char>) -> bool {
        match ch {
            ch if self.is_follow_value(ch) => true,
            Some(':') => true,
            _ => false,
        }
    }

    fn string(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.next() != Some('"') {
            return Err("invalid string value".to_string());
        }

        let mut value = String::new();

        while let c @ Some('\u{0020}'..='\u{10FFFF}') = ctx.next() {
            if c == Some('"') {
                break;
            }
            if c == Some('\\') {
                // TODO: escape
            }
            value.push(c.unwrap());
        }

        if !self.is_follow_string(ctx.peek()) {
            return Err("invalid string".to_string())
        }

        Ok(vec![Token::String(value)])
    }

    fn number(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        Ok(vec![vec![Token::NumberStart], self.integer(ctx)?, self.fraction(ctx)?, self.exponent(ctx)?, vec![Token::NumberEnd]].into_iter().flatten().collect::<Vec<Token>>())
    }

    fn integer(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        let mut negative = false;
        if ctx.peek() == Some('-') {
            ctx.next();
            negative = true;
        }

        let mut value: i64 = 0;

        while match ctx.peek() {
            ch if self.is_ws(ch) => false,
            Some('.') | Some('e') | Some('E') | Some(',')  => false,
            _ => true,
        } {
            if let v @ Some('0'..='9') = ctx.peek() {
                value *= 10;
                value += v.unwrap() as i64 - '0' as i64;
            } else {
                return Err("invalid integer".to_string());
            }

            ctx.next();
        }

        if negative {
            value *= -1;
        }

        Ok(vec![Token::Integer(value)])
    }

    fn fraction(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.peek() != Some('.') {
            return Ok(vec![]);
        }

        ctx.next();

        let mut value: i64 = 0;

        while match ctx.peek() {
            ch if self.is_ws(ch) => false,
            Some('e') | Some('E') | Some(',') => false,
            _ => true,
        } {
            if let v @ Some('0'..='9') = ctx.peek() {
                value *= 10;
                value += v.unwrap() as i64 - '0' as i64;
            } else {
                return Err("invalid fraction".to_string());
            }

            ctx.next();
        }

        Ok(vec![Token::Fraction(value)])
    }

    fn exponent(&self, ctx: &mut Context) -> Result<Vec<Token>, String> {
        if ctx.peek() != Some('e') && ctx.peek() != Some('E') {
            return Ok(vec![]);
        }

        ctx.next();

        let mut negative = false;
        if ctx.peek() == Some('-') {
            ctx.next();
            negative = true;
        }

        let mut value: i64 = 0;

        while match ctx.peek() {
            ch if self.is_ws(ch) => false,
            Some(',')  => false,
            _ => true,
        } {
            if let v @ Some('0'..='9') = ctx.peek() {
                value *= 10;
                value += v.unwrap() as i64 - '0' as i64;
            } else {
                return Err("invalid exponent".to_string());
            }

            ctx.next();
        }

        if negative {
            value *= -1;
        }

        Ok(vec![Token::Exponent(value)])
    }
}

#[derive(Clone,Debug)]
enum Value {
    JsonValue(Box<Value>),
    StringValue(String),
    NumberValue(f64),
    BoolValue(bool),
    NullValue,
    ArrayValue(Vec<Value>),
    ObjectValue(HashMap<String, Value>),
}

struct ParserContext<'t> {
    tokens: &'t Vec<Token>,
    stack: Vec<Value>,
}

impl<'t> ParserContext<'t> {
    pub fn new(tokens: &'t Vec<Token>) -> ParserContext {
        ParserContext {
            tokens,
            stack: vec![],
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }
    
    pub fn peek(&self) -> Option<&Value> {
        self.stack.last()
    }
}

struct Parser {}

impl Parser {
    pub fn parse(&self, ctx: &mut ParserContext) -> Result<Value, String> {
        for token in ctx.tokens {
            match token {
                Token::JsonStart => self.json_start(ctx)?,
                Token::ValueStart => self.value_start(ctx)?,
                Token::ObjectStart => self.object_start(ctx)?,
                Token::MemberStart => self.member_start(ctx)?,
                Token::String(value) => self.string(ctx, value)?,
                Token::NumberStart => self.number_start(ctx)?,
                Token::Integer(value) => self.integer(ctx, *value)?,
                Token::Fraction(value) => self.fraction(ctx, *value)?,
                Token::Exponent(value) => self.exponent(ctx, *value)?,
                Token::NumberEnd => self.number_end(ctx)?,
                Token::MemberEnd => self.member_end(ctx)?,
                Token::ObjectEnd => self.object_end(ctx)?,
                Token::ValueEnd => self.value_end(ctx)?,
                Token::JsonEnd => self.json_end(ctx)?,
                Token::Bool(value) => self.bool(ctx, *value)?,
                _ => println!("unhandled {:?}", token),
            }
        }

        let json = (match ctx.pop() {
            Some(Value::JsonValue(json)) => Ok(json),
            _ => Err("expected json".to_string()),
        })?;
        
        Ok(*json)
    }

    fn json_start(&self, ctx: &mut ParserContext) -> Result<(), String> {
        ctx.push(Value::JsonValue(Box::new(Value::NullValue)));
        Ok(())
    }

    fn value_start(&self, ctx: &mut ParserContext) -> Result<(), String> {
        Ok(())
    }

    fn object_start(&self, ctx: &mut ParserContext) -> Result<(), String> {
        ctx.push(Value::ObjectValue(HashMap::new()));
        Ok(())
    }

    fn member_start(&self, ctx: &mut ParserContext) -> Result<(), String> {
        Ok(())
    }

    fn string(&self, ctx: &mut ParserContext, value: &String) -> Result<(), String> {
        ctx.push(Value::StringValue(value.clone()));
        Ok(())
    }

    fn bool(&self, ctx: &mut ParserContext, value: bool) -> Result<(), String> {
        ctx.push(Value::BoolValue(value));
        Ok(())
    }

    fn number_start(&self, ctx: &mut ParserContext) -> Result<(), String> {
        ctx.push(Value::NumberValue(0f64));
        Ok(())
    }

    fn integer(&self, ctx: &mut ParserContext, value: i64) -> Result<(), String> {
        let lvalue = (match ctx.pop() {
            Some(Value::NumberValue(_)) => Ok(Value::NumberValue(value as f64)),
            _ => Err("expected number".to_string()),
        })?;

        ctx.push(lvalue);

        Ok(())
    }

    fn fraction(&self, ctx: &mut ParserContext, value: i64) -> Result<(), String> {
        let v = value as f64;
        let frac = v / 10f64.powf(v.log10() + 1f64);

        let lvalue = (match ctx.pop() {
            Some(Value::NumberValue(int)) => Ok(Value::NumberValue(int + frac)), 
            _ => Err("expected number".to_string()),
        })?;

        ctx.push(lvalue);

        Ok(())
    }

    fn exponent(&self, ctx: &mut ParserContext, value: i64) -> Result<(), String> {
        let v = value as f64;

        let lvalue = (match ctx.pop() {
            Some(Value::NumberValue(f)) => Ok(Value::NumberValue(f * 10f64.powf(v))),
            _ => Err("expected number".to_string()),
        })?;

        ctx.push(lvalue);

        Ok(())
    }

    fn number_end(&self, ctx: &mut ParserContext) -> Result<(), String> {
        let number = (match ctx.pop() {
            Some(Value::NumberValue(v)) => Ok(v),
            _ => Err("expected number".to_string()),
        })?;

        if let Some(Value::ArrayValue(array)) = ctx.peek() {
            ctx.push(Value::ArrayValue(vec![
                    array.clone(),
                    vec![Value::NumberValue(number)],
            ].into_iter().flatten().collect::<Vec<_>>()))
        } else {
            ctx.push(Value::NumberValue(number));
        }

        Ok(())
    }

    fn member_end(&self, ctx: &mut ParserContext) -> Result<(), String> {
        let rvalue = (match ctx.pop() {
            Some(v) => Ok(v),
            None => Err("expected value".to_string()),
        })?;

        let id = (match ctx.pop() {
            Some(Value::StringValue(id)) => Ok(id),
            _ => Err("expected string".to_string()),
        })?;

        let lvalue = (match ctx.pop() {
            Some(Value::ObjectValue(obj)) => {
                let mut new = obj.clone();
                new.insert(id, rvalue);
                Ok(Value::ObjectValue(new))
            },
            _ => Err("expected object".to_string()),
        })?;

        ctx.push(lvalue);

        Ok(())
    }

    fn value_end(&self, ctx: &mut ParserContext) -> Result<(), String> {
        Ok(())
    }

    fn object_end(&self, ctx: &mut ParserContext) -> Result<(), String> {
        let rvalue = ctx.pop().unwrap();

        let lvalue = (match ctx.pop() {
            Some(Value::JsonValue(_)) => Ok(Value::JsonValue(Box::new(rvalue))),
            Some(Value::ArrayValue(array)) => Ok(Value::ArrayValue(vec![array, vec![rvalue]].into_iter().flatten().collect::<Vec<_>>())),
            _ => Err("expected json or array".to_string()),
        })?;

        ctx.push(lvalue);

        Ok(())
    }

    fn json_end(&self, ctx: &mut ParserContext) -> Result<(), String> {
        Ok(())
    }
}

fn main() {
    let json = "{ \"a\": true, \"b\": \"hello\", \"c\": 1, \"d\": { \"e\": 100.0 } }";

    let mut ctx = Context::new(json);
    let lexer = Lexer::new();

    let tokens = lexer.tokenize(&mut ctx);
    if let Err(e) = tokens {
        println!("{}", e);
        println!("{}", ctx.debug());
        return;
    }

    let tokens = tokens.unwrap();

    for token in &tokens {
        println!("{:?}", token);
    }

    let mut parserCtx = ParserContext::new(&tokens);
    let parser = Parser {};

    println!("{:?}", parser.parse(&mut parserCtx));
}
