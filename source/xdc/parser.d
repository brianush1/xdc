module xdc.parser;
import xdc.source;
import xdc.metadata;
import sumtype;
import std.typecons;
import std.algorithm;
import std.range;
import std.utf;
import std.utf : stride, count;
import std.conv;

immutable(string[]) SYMBOLS = [
	"/", "/=", ".", "..", "...", "&", "&=", "&&", "|", "|=", "||", "-", "-=", "--", "+", "+=", "++", "<", "<=", "<<", "<<=", ">", ">=", ">>=", ">>>=", ">>", ">>>", "!", "!=", "(", ")", "[", "]", "{", "}", "?", ",", ";", ":", "$", "=", "==", "*", "*=", "%", "%=", "^", "^=", "^^", "^^=", "~", "~=", "@", "=>", "#",
].sort!((a, b) => b.count < a.count).array;

static foreach (i, symbol; SYMBOLS) {
	template Symbol(string T) if (T == symbol) {
		enum Symbol = cast(ushort) i;
	}
}

immutable(string[]) KEYWORDS = [
	"__FILE__", "__FILE_FULL_PATH__", "__FUNCTION__", "__gshared", "__LINE__", "__MODULE__", "__parameters", "__PRETTY_FUNCTION__", "__traits", "__vector", "abstract", "alias", "align", "asm", "assert", "auto", "body", "bool", "break", "byte", "case", "cast", "catch", "cdouble", "cent", "cfloat", "char", "class", "const", "continue", "creal", "dchar", "debug", "default", "delegate", "delete", "deprecated", "do", "double", "else", "enum", "export", "extern", "false", "final", "finally", "float", "for", "foreach", "foreach_reverse", "function", "goto", "idouble", "if", "ifloat", "immutable", "import", "in", "inout", "int", "interface", "invariant", "ireal", "is", "lazy", "long", "macro", "mixin", "module", "new", "nothrow", "null", "out", "override", "package", "pragma", "private", "protected", "public", "pure", "real", "ref", "return", "scope", "shared", "short", "static", "struct", "super", "switch", "synchronized", "template", "this", "throw", "true", "try", "typeid", "typeof", "ubyte", "ucent", "uint", "ulong", "union", "unittest", "ushort", "version", "void", "wchar", "while", "with",
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

	struct UInt {
		uint value;
	}

	struct ULong {
		ulong value;
	}

	struct Int {
		int value;
	}

	struct Long {
		long value;
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
		UInt,
		ULong,
		Int,
		Long,
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
			(UInt token) => "uint '" ~ token.value.to!string ~ "'",
			(ULong token) => "ulong '" ~ token.value.to!string ~ "'",
			(Int token) => "int '" ~ token.value.to!string ~ "'",
			(Long token) => "long '" ~ token.value.to!string ~ "'",
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

	static bool isIdentifierStart(dchar c) {
		return c == '_' || (c >= 'a' && c <= 'z')
			|| (c >= 'A' && c <= 'Z');
	}

	static bool isIdentifier(dchar c) {
		return isIdentifierStart(c) || (c >= '0' && c <= '9');
	}

}

class Lexer {
	Diagnostic[] diagnostics;

	this(Source source) {
		this.source = source;
	}

private:

	this() {}

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
		if (index >= source.length) {
			return 0;
		}
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
				diagnostics ~= Diagnostic(
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
				diagnostics ~= Diagnostic(
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
		else if (first == '"') {
			// TODO: proper escapes and wysiwyg/hex/token/delimited strings
			nextChar();
			bool escape = false;
			string value = readWhile((c) {
				if (escape) {
					escape = false;
					return true;
				}
				else if (c == '\\') {
					escape = true;
					return true;
				}
				else if (c == '"') {
					return false;
				}

				return true;
			});
			if (eof) {
				diagnostics ~= Diagnostic(
					Diagnostic.Kind.Error,
					Nullable!Span(Span(source, index, index)),
					"unclosed string",
					"expected (\") here, not EOF",
					0,
					[
						Diagnostic.Extra(
							Diagnostic.Extra.Kind.Note,
							"string starts here",
							Nullable!Span(Span(source, start, start + 1)),
						),
					],
				);
			}
			nextChar();
			auto suffix = Token.String.Suffix.Implicit;
			if (peekChar == 'c') {
				nextChar();
				suffix = Token.String.Suffix.Char;
			}
			else if (peekChar == 'w') {
				nextChar();
				suffix = Token.String.Suffix.Wchar;
			}
			else if (peekChar == 'd') {
				nextChar();
				suffix = Token.String.Suffix.Dchar;
			}
			result.payload = Token.String(value, suffix);
		}
		else if (Chars.isIdentifierStart(first)) {
			string value = readWhile(c => Chars.isIdentifier(c));
			if (value == "__EOF__") {
				result.payload = Token.Eof();
			}
			else if (value == "__VENDOR__") {
				result.payload = Token.String(Vendor);
			}
			else if (value == "__VERSION__") {
				result.payload = Token.Long(Version[0] * 1000 + Version[1]);
			}
			else if (value == "__DATE__") {
				import std.datetime.systime : SysTime, Clock;

				SysTime time = Clock.currTime;
				string month = ["", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"][time.month];
				string date = time.day.to!string;
				if (date.length == 0)
					date = "0" ~ date;
				string year = time.year.to!string;
				result.payload = Token.String(month ~ " " ~ date ~ " " ~ year, Token.String.Suffix.Implicit);
			}
			else if (value == "__TIME__") {
				import std.datetime.systime : SysTime, Clock;
				import std.format : format;

				SysTime time = Clock.currTime;
				string hour = time.hour.format!"%02s";
				string min = time.minute.format!"%02s";
				string sec = time.second.format!"%02s";
				result.payload = Token.String(hour ~ ":" ~ min ~ ":" ~ sec, Token.String.Suffix.Implicit);
			}
			else if (value == "__TIMESTAMP__") {
				import std.datetime.systime : SysTime, Clock;
				import std.format : format;

				SysTime time = Clock.currTime;
				string hour = time.hour.format!"%02s";
				string min = time.minute.format!"%02s";
				string sec = time.second.format!"%02s";
				string month = ["", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"][time.month];
				string day = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"][time.dayOfWeek];
				string date = time.day.to!string;
				if (date.length == 0)
					date = "0" ~ date;
				string year = time.year.to!string;
				result.payload = Token.String(day ~ " " ~ month ~ " " ~ date ~ " " ~ hour ~ ":" ~ min ~ ":" ~ sec ~ " " ~ year, Token.String.Suffix.Implicit);
			}
			else if (KEYWORDS.canFind(value)) {
				result.payload = Token.Keyword(value);
			}
			else {
				result.payload = Token.Identifier(value);
			}
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
		auto result = new Lexer;
		result.diagnostics = diagnostics;
		result.source = source;
		result.peekedToken = peekedToken;
		result.index = index;
		return result;
	}

}

unittest {
	auto src = new Source("file.d", q"( /+ a /+ b +/ c+/ d )");
	Lexer lexer = new Lexer(src);
	assert(lexer.front == Token(
		Span(src, 1, 17),
		Token.Payload(Token.NestingBlockComment(" a /+ b +/ c")),
	));
}

unittest {
	auto src = new Source("file.d", q"( /* a /* b */ c*/ d )");
	Lexer lexer = new Lexer(src);
	assert(lexer.front == Token(
		Span(src, 1, 13),
		Token.Payload(Token.BlockComment(" a /* b ")),
	));
}

unittest {
	auto src = new Source("file.d", q"( &&&= )");
	Lexer lexer = new Lexer(src);
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

unittest {
	auto src = new Source("file.d", q"( a123 _754 _ int x bool )");
	Lexer lexer = new Lexer(src);
	assert(lexer.front == Token(
		Span(src, 1, 5),
		Token.Payload(Token.Identifier("a123")),
	));
	lexer.popFront;
	assert(lexer.front == Token(
		Span(src, 6, 10),
		Token.Payload(Token.Identifier("_754")),
	));
	lexer.popFront;
	assert(lexer.front == Token(
		Span(src, 11, 12),
		Token.Payload(Token.Identifier("_")),
	));
	lexer.popFront;
	assert(lexer.front == Token(
		Span(src, 13, 16),
		Token.Payload(Token.Keyword("int")),
	));
	lexer.popFront;
	assert(lexer.front == Token(
		Span(src, 17, 18),
		Token.Payload(Token.Identifier("x")),
	));
	lexer.popFront;
	assert(lexer.front == Token(
		Span(src, 19, 23),
		Token.Payload(Token.Keyword("bool")),
	));
}

unittest {
	import std.algorithm : map;

	auto src = new Source("file.d", q"( __DATE__ __TIME__ __TIMESTAMP__ __VENDOR__ __VERSION__ __EOF__ asdg/ g;hd'gdgh )");
	Lexer lexer = new Lexer(src);
	assert(lexer.map!(x => x.span).array == [
		Span(src, 1, 9),
		Span(src, 10, 18),
		Span(src, 19, 32),
		Span(src, 33, 43),
		Span(src, 44, 55),
	]);
	assert(lexer.front == Token(
		Span(src, 56, 63),
		Token.Payload(Token.Eof()),
	));
}
