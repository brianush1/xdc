module xdc.parser;
import xdc.source;
import sumtype;
import std.typecons;
import std.algorithm;
import std.range;
import std.utf;
import std.utf : stride, count;
import std.conv;

immutable(string[]) SYMBOLS = [
	"/", "/=", ".", "..", "...", "&", "&=", "&&", "|",
	"|=", "||", "-", "-=", "--", "+", "+=", "++", "<",
	"<=", "<<", "<<=", ">", ">=", ">>=", ">>>=", ">>",
	">>>", "!", "!=", "(", ")", "[", "]", "{", "}", "?",
	",", ";", ":", "$", "=", "==", "*", "*=", "%", "%=",
	"^", "^=", "^^", "^^=", "~", "~=", "@", "=>", "#",
].sort!((a, b) => b.count < a.count).array;

static foreach (i, symbol; SYMBOLS) {
	template Symbol(string T) if (T == symbol) {
		enum Symbol = cast(ushort) i;
	}
}

immutable(string[]) KEYWORDS = [
	"__FILE__", "__FILE_FULL_PATH__", "__FUNCTION__", "__gshared",
	"__LINE__", "__MODULE__", "__parameters", "__PRETTY_FUNCTION__",
	"__traits", "__vector", "abstract", "alias", "align", "asm",
	"assert", "auto", "body", "bool", "break", "byte", "case", "cast",
	"catch", "cdouble", "cent", "cfloat", "char", "class", "const",
	"continue", "creal", "dchar", "debug", "default", "delegate",
	"delete", "deprecated", "do", "double", "else", "enum", "export",
	"extern", "false", "final", "finally", "float", "for", "foreach",
	"foreach_reverse", "function", "goto", "idouble", "if", "ifloat",
	"immutable", "import", "in", "inout", "int", "interface", "invariant",
	"ireal", "is", "lazy", "long", "macro", "mixin", "module", "new",
	"nothrow", "null", "out", "override", "package", "pragma", "private",
	"protected", "public", "pure", "real", "ref", "return", "scope",
	"shared", "short", "static", "struct", "super", "switch",
	"synchronized", "template", "this", "throw", "true", "try", "typeid",
	"typeof", "ubyte", "ucent", "uint", "ulong", "union", "unittest",
	"ushort", "version", "void", "wchar", "while", "with",
];

struct Token {
	struct Eof {}

	struct Unknown {
		string value;
	}

	struct Identifier {
		string name;
	}

	struct Keyword {
		string name;
	}

	struct String {
		enum Suffix {
			Implicit,
			Char,
			Wchar,
			Dchar,
		}

		string value;
		Suffix suffix;
	}

	struct Char {
		dchar value;
	}

	struct Integer {
		enum Suffix {
			Implicit = 0,
			Long = 1,
			Unsigned = 2,
			LU = Long | Unsigned,
		}

		ulong value;
		Suffix suffix;
	}

	struct Float {
		float value;
	}

	struct Double {
		double value;
	}

	struct Symbol {
		ushort id;
	}

	struct LineComment {
		string value;
	}

	struct BlockComment {
		string value;
	}

	struct NestingBlockComment {
		string value;
	}

	Span span;
	alias Payload = SumType!(
		Eof,
		Unknown,
		Identifier,
		Keyword,
		String,
		Char,
		Integer,
		Float,
		Double,
		Symbol,
		LineComment,
		BlockComment,
		NestingBlockComment,
	);
	Payload payload;
	alias payload this;

	string toString() const {
		return payload.match!(
			(Eof token) => "<eof>",
			(Unknown token) => "unknown '" ~ token.value.to!string ~ "'",
			(Identifier token) => "identifier '" ~ token.name ~ "'",
			(Keyword token) => "keyword '" ~ token.name ~ "'",
			(String token) => "string \"" ~ token.value ~ "\"", // TODO: escapes
			(Char token) => "char '" ~ token.value.to!string ~ "'",
			(Integer token) => "integer '" ~ token.value.to!string ~ "'",
			(Float token) => "float '" ~ token.value.to!string ~ "'",
			(Double token) => "double '" ~ token.value.to!string ~ "'",
			(Symbol token) => "symbol '" ~ SYMBOLS[token.id].to!string ~ "'",
			(LineComment token) => "comment '" ~ token.value ~ "'",
			(BlockComment token) => "block comment '" ~ token.value ~ "'",
			(NestingBlockComment token) => "nesting comment '" ~ token.value ~ "'",
		);
	}
}

private struct Chars {

	static bool isSpace(dchar c) {
		return c == ' ' || c == '\t' || c == '\u000B' || c == '\u000C'
			|| c == '\n' || c == '\r';
	}

}

struct Lexer {
	this(Source source, ref Diagnostic[] diagnostics) {
		this.source = source;
		this.diagnostics = &diagnostics;
	}

private:

	Diagnostic[]* diagnostics;

	Source source;

	Nullable!Token peekedToken;

	size_t index;

	dchar peekChar() {
		if (index >= source.length) {
			return 0;
		}
		return source[index .. $].front;
	}

	string peekChars(size_t length) {
		if (index >= source.length)
			return "";
		string s = source[index .. $];
		size_t at;
		foreach (i; 0 .. length) {
			if (at >= s.length)
				return "";
			at += s.stride(at);
		}
		return source[index .. index + at];
	}

	dchar nextChar() {
		dchar result = peekChar;
		index += source.stride(index);
		return result;
	}

	string nextChars(size_t length) {
		if (index >= source.length)
			return "";
		string s = source[index .. $];
		size_t at;
		foreach (i; 0 .. length) {
			if (at >= s.length)
				return "";
			at += s.stride(at);
		}
		index += at;
		return source[index - at .. index];
	}

	bool eof() {
		return peekChar == 0;
	}

	string readWhile(bool delegate(dchar) predicate) {
		size_t start = index;
		while (!eof && predicate(peekChar)) {
			nextChar();
		}
		return source[start .. index];
	}

	void frontImpl() {
		Token result;

		readWhile(c => Chars.isSpace(c));

		size_t start = index;

		dchar first = peekChar;

		if (first == 0) {
			result.payload = Token.Eof();
		}
		else if (peekChars(2) == "//") {
			nextChars(2);
			string value = readWhile(c => c != '\n');
			result.payload = Token.LineComment(value);
		}
		else if (peekChars(2) == "/*") {
			nextChars(2);
			string value = readWhile(c => peekChars(2) != "*/");
			if (eof) {
				*diagnostics ~= Diagnostic(
					Diagnostic.Kind.Error,
					Nullable!Span(Span(source, index, index)),
					"unclosed block comment",
					"expected '*/' here, not EOF",
					0,
					[
						Diagnostic.Extra(
							Diagnostic.Extra.Kind.Note,
							"comment starts here",
							Nullable!Span(Span(source, start, start + 2)),
						)
					],
				);
			}
			nextChars(2);
			result.payload = Token.BlockComment(value);
		}
		else if (peekChars(2) == "/+") {
			nextChars(2);
			int nestingCount = 1;
			string value = readWhile((c) {
				if (peekChars(2) == "/+") {
					nestingCount += 1;
				}
				else if (peekChars(2) == "+/") {
					nestingCount -= 1;
				}

				return nestingCount > 0;
			});
			if (eof) {
				*diagnostics ~= Diagnostic(
					Diagnostic.Kind.Error,
					Nullable!Span(Span(source, index, index)),
					"unclosed nesting block comment",
					"expected '+/' here, not EOF",
					0,
					[
						Diagnostic.Extra(
							Diagnostic.Extra.Kind.Note,
							"comment starts here",
							Nullable!Span(Span(source, start, start + 2)),
						),
					],
				);
			}
			nextChars(2);
			result.payload = Token.NestingBlockComment(value);
		}
		else {
			foreach (i, symbol; SYMBOLS) {
				if (peekChars(symbol.count) == symbol) {
					nextChars(symbol.count);
					result.payload = Token.Symbol(cast(ushort) i);
					goto foundOne;
				}
			}
			result.payload = Token.Unknown(nextChars(1));
			foundOne:
		}

		result.span = Span(source, start, index);
		peekedToken = result;
	}

public:

	Token front() {
		if (peekedToken.isNull)
			frontImpl();
		return peekedToken.get;
	}

	void popFront() {
		if (peekedToken.isNull)
			frontImpl();
		if (!empty)
			peekedToken = Nullable!Token.init;
	}

	bool empty() {
		return front.payload.match!((Token.Eof _) => true, _ => false);
	}

	Lexer save() {
		return this;
	}

}

unittest {
	auto src = new Source("file.d", q"( /+ a /+ b +/ c+/ d )");
	Diagnostic[] diagnostics;
	Lexer lexer = Lexer(src, diagnostics);
	assert(lexer.front == Token(
		Span(src, 1, 17),
		Token.Payload(Token.NestingBlockComment(" a /+ b +/ c")),
	));
}

unittest {
	auto src = new Source("file.d", q"( /* a /* b */ c*/ d )");
	Diagnostic[] diagnostics;
	Lexer lexer = Lexer(src, diagnostics);
	assert(lexer.front == Token(
		Span(src, 1, 13),
		Token.Payload(Token.BlockComment(" a /* b ")),
	));
}

unittest {
	auto src = new Source("file.d", q"( &&&= )");
	Diagnostic[] diagnostics;
	Lexer lexer = Lexer(src, diagnostics);
	assert(lexer.front == Token(
		Span(src, 1, 3),
		Token.Payload(Token.Symbol(Symbol!"&&")),
	));
	lexer.popFront;
	assert(lexer.front == Token(
		Span(src, 3, 5),
		Token.Payload(Token.Symbol(Symbol!"&=")),
	));
	lexer.popFront;
	assert(lexer.front == Token(
		Span(src, 6, 6),
		Token.Payload(Token.Eof()),
	));
}
