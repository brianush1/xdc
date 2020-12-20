module xdc.source;

/** Represents a span from the given start index to the end index in a source */
struct Span {
	Source source;
	size_t start;
	size_t end;
}

/** Represents a place in the source in between characters, much like a text editor cursor */
struct Place {
	/** The 0-indexed line that this place refers to */
	size_t line;

	/** The number of bytes that come before this place on its line */
	size_t offset;
}

final class Source {

	/** An array containing the starting indices of each line in the source */
	private size_t[] lineIndices;

	immutable(string) content;

	immutable(string) file;

	alias content this;

	this(string file, string content) {
		this.file = file;
		this.content = content;

		lineIndices ~= 0;
		foreach (i, dchar c; content) {
			if (c == '\n') {
				lineIndices ~= i + 1;
			}
		}
	}

	private size_t lineEnd(size_t i) const {
		if (i + 1 == lineIndices.length) {
			return content.length;
		}
		else {
			return lineIndices[i + 1] - 1;
		}
	}

	/** Convenience function: lineIndices[i] */
	private size_t lineStart(size_t i) const {
		return lineIndices[i];
	}

	/** Returns a place from a given index in the source */
	Place placeFromIndex(size_t index) const {
		Place result;
		assert(lineIndices.length > 0);
		size_t low = 0;
		size_t high = lineIndices.length - 1; // @suppress(dscanner.suspicious.length_subtraction)
		while (true) {
			size_t mid = (low + high) / 2;
			if (lineEnd(mid) < index) {
				low = mid + 1;
			}
			else if (lineStart(mid) > index) {
				high = mid - 1;
			}
			else {
				result.line = mid;
				break;
			}
		}
		result.offset = index - lineStart(result.line);
		return result;
	}

	string line(size_t index) const {
		return content[lineStart(index) .. lineEnd(index)];
	}

	size_t lines() const @property {
		return lineIndices.length;
	}

}

struct Diagnostic {
	import std.typecons : Nullable, Tuple, tuple;
	import std.conv : to;
	import std.regex : ctRegex, replaceAll;

	private {
		enum TabSize = 4;
		enum GutterPadLeft = 1;
		enum GutterPadRight = 1;
		enum PadLeft = 1;
		enum PadTop = 1;
		enum PadBottom = 0;
	}

	enum Kind {
		Error,
		Warning,
		Note,
		Help,
		InternalError,
	}

	struct Extra {
		enum Kind {
			Note,
			Help,
		}

		Kind kind;
		string message;
		Nullable!Span span;
	}

	Kind kind;
	Nullable!Span span;
	string message;
	string inlineMessage;
	uint code;

	Extra[] extra;

	void report() {
		import arsd.terminal : Terminal, ConsoleOutputType, Color, Bright;
		import std.range : repeat;

		// TODO: graphemes?
		// TODO: replace arsd.terminal with smaller coloring boilerplate

		auto term = Terminal(ConsoleOutputType.linear);

		int width = term.width;

		enum Art : dchar {
			TL = '╭',
			TR = '╮',
			BL = '╰',
			BR = '╯',
			T = '─',
			B = T,
			L = '│',
			R = L,
			TLL = '╴',
			VEllipsis = ':',
			Bullet = '•',
		}

		struct Char {
			dchar c = 0;
			Color fg = Color.DEFAULT;
			Color bg = Color.DEFAULT;
		}

		Char[][] contents;

		ref Char at(size_t row, size_t col) {
			static Char dummy;
			if (col >= width) {
				return dummy;
			}
			while (row >= contents.length) {
				contents ~= new Char[width];
			}
			return contents[row][col];
		}

		dstring[] wrap(string str, int maxWidth) {
			import std.regex : replaceAll, ctRegex;

			// FIXME: wrap properly, breaking on whitespace/non-alphanumeric chars

			if (maxWidth <= 0)
				return [];

			dchar[][] result;
			int row, col;
			foreach (dchar c; str) {
				if (c == '\n') {
					row += 1;
					col = 0;
				}
				else if (c == '\t') {
					col = (col + TabSize) / TabSize * TabSize;
				}
				else {
					while (row >= result.length)
						result ~= cast(dchar[]) [];
					while (col >= result[row].length)
						result[row] ~= 0;
					result[row][col] = c;
					col += 1;
				}
				if (col >= maxWidth) {
					row += 1;
					col = 0;
				}
			}
			return cast(dstring[]) result;
		}

		/** Returns: (rows, cols) */
		Tuple!(int, int) measure(string str, int maxWidth) {
			import std.algorithm : max;

			dstring[] lines = wrap(str, maxWidth);
			if (lines == []) return tuple(0, 0);

			int cols;
			foreach (line; lines) {
				cols = max(cols, cast(int) line.length);
			}
			return tuple(cast(int) lines.length, cols);
		}

		Tuple!(int, int) putw(string str, int row, int col,
				Color fg, Color bg, int maxWidth) {
			dstring[] lines = wrap(str, maxWidth);
			if (lines == []) return tuple(row, col);

			foreach (i, line; lines) {
				foreach (j, c; line) {
					at(row + i, col + j) = Char(c, fg, bg);
				}
			}
			return tuple(row + cast(int) lines.length - 1,
				col + cast(int) lines[$ - 1].length);
		}

		Tuple!(int, int) put(string str, int row, int col,
				Color fg, Color bg) {
			return putw(str, row, col, fg, bg, width - col);
		}

		Color bright(Color color) {
			return cast(Color)(Bright | color);
		}

		auto color = Color.red;

		auto codeStr = code.to!string;
		auto afterError = put(
			kind.to!string.replaceAll(ctRegex!r"([A-Z])", " $1")[1 .. $]
			~ "(" ~ kind.to!string.replaceAll(ctRegex!r"[^A-Z]", "")
			~ '0'.repeat(4 - codeStr.length).to!string ~ codeStr ~ ")"
			~ ": ",
			0, 0, bright(color), Color.DEFAULT,
		);

		auto afterMsg = put(
			message,
			afterError[0], afterError[1], bright(Color.DEFAULT), Color.DEFAULT,
		);

		int atRow = afterMsg[0] + 1;

		struct SpanReport {
			enum Kind {
				/** the span is the main span of this diagnostic */
				It,
				Note,
				Help,
			}

			Kind kind;
			size_t start;
			size_t end;
			string message;
		}

		int countSpaces(string s) {
			int result;
			foreach (dchar c; s) {
				if (c == '\t') {
					result = (result + TabSize) / TabSize * TabSize;
				}
				else {
					result += 1;
				}
			}
			return result;
		}

		void reportSpans(Source source, SpanReport[] spans) {
			import std.algorithm : sort, max;
			import std.array : array;

			int gutter = 1;

			struct Line {
				/** 0-indexed */
				size_t index;

				SpanReport[] spans;
			}

			Line[size_t] lineMap;
			bool[size_t] lineSet;

			foreach (span; spans) {
				Place start = source.placeFromIndex(span.start);
				Place end = source.placeFromIndex(span.end);
				foreach (i; start.line .. end.line + 1) {
					if (i !in lineMap) {
						Line line = {
							index: i,
							spans: [],
						};
						gutter = max(gutter, cast(int)(i + 1).to!string.length);
						lineMap[i] = line;
						lineSet[i] = true;
					}
					lineMap[i].spans ~= span;
				}
			}

			size_t[] lines;

			bool isEmpty(string s) {
				foreach (dchar c; s) {
					if (!(c == ' ' || c == '\t' || c == '\u000B' || c == '\u000C' || c == '\n' || c == '\r')) {
						return false;
					}
				}
				return true;
			}

			foreach (line; lineSet.byKey.array) {
				if (line > 0 && !isEmpty(source.line(line - 1))) {
					lineSet[line - 1] = true;
				}
				if (line < source.lines - 1 && !isEmpty(source.line(line + 1))) {
					lineSet[line + 1] = true;
				}
			}

			foreach (line; lineSet.byKey) {
				lines ~= line;
			}

			lines.sort();

			for (size_t i = 0; i + 1 < lines.length; i += 1) {
				if (lines[i] == lines[i + 1] - 2) {
					lines = lines[0 .. i + 1] ~ (lines[i] + 1) ~ lines[i + 1 .. $];
				}
			}

			foreach (line; lines) {
				if (line !in lineMap) {
					Line lineData = {
						index: line,
						spans: [],
					};
					gutter = max(gutter, cast(int)(line + 1).to!string.length);
					lineMap[line] = lineData;
				}
			}

			gutter += GutterPadLeft + GutterPadRight;

			at(atRow, gutter) = Char(Art.TL, color);
			at(atRow, gutter + 1) = Char(Art.T, color);

			auto gutterStartRow = atRow;

			auto afterFile = put(
				source.file,
				atRow, gutter + 3, Color.DEFAULT, Color.DEFAULT,
			);

			atRow = afterFile[0];

			at(atRow, afterFile[1] + 1) = Char(Art.T, color);
			at(atRow, afterFile[1] + 2) = Char(Art.TLL, color);

			atRow += 1 + PadTop;

			foreach (i, lineIndex; lines) {
				auto line = lineMap[lineIndex];
				string numStr = (line.index + 1).to!string;
				put(numStr, atRow, gutter - GutterPadRight - cast(int) numStr.length, color, Color.DEFAULT);
				string lineStr = source.line(line.index);
				put(lineStr, atRow, gutter + 1 + PadLeft, Color.DEFAULT, Color.DEFAULT);
				atRow += 1;
				scope (exit) {
					if (i + 1 < lines.length && lines[i + 1] != lines[i] + 1) {
						at(atRow, gutter - GutterPadRight - 1) = Char(Art.VEllipsis, color);
						atRow += 1;
					}
				}
				if (line.spans.length == 0)
					continue;
				int squigglyRow = atRow;
				atRow += 1;
				// TODO: nicer multi-line error reporting
				// TODO: syntax highlighting in error reporting
				// FIXME: overlapping spans
				// FIXME: make it work when lines wrap around
				bool oneSpan = line.spans.length == 1;
				line.spans.sort!((a, b) {
					size_t get(alias v)() {
						Place place = source.placeFromIndex(v.start);
						size_t start = place.line == line.index ? place.offset : 0;
						return countSpaces(lineStr[0 .. start]);
					}

					return get!a > get!b;
				});
				int[] messageLines = new int[width];
				foreach (span; line.spans) {
					Place startPlace = source.placeFromIndex(span.start);
					Place endPlace = source.placeFromIndex(span.end);
					size_t start = startPlace.line == line.index ? startPlace.offset : 0;
					size_t end = endPlace.line == line.index ? endPlace.offset : lineStr.length;
					int startc = countSpaces(lineStr[0 .. start]);
					int endc = startc + countSpaces(lineStr[start .. end]);
					int startCol = gutter + 1 + PadLeft + startc;
					Color squigglyColor;
					final switch (span.kind) {
					case SpanReport.Kind.It:
						squigglyColor = color;
						break;
					case SpanReport.Kind.Help:
						squigglyColor = Color.blue;
						break;
					case SpanReport.Kind.Note:
						squigglyColor = Color.white;
						break;
					}
					Tuple!(int, int) squigglyEnd;
					if (start == end) {
						squigglyEnd = put("^", squigglyRow, startCol, squigglyColor, Color.DEFAULT);
					}
					else {
						squigglyEnd = put('~'.repeat(endc - startc).to!string, squigglyRow, startCol, squigglyColor, Color.DEFAULT);
					}

					if (oneSpan) {
						auto msgEnd = put(span.message, squigglyEnd[0], squigglyEnd[1] + 1, squigglyColor, Color.DEFAULT);
						atRow = msgEnd[0] + 1;
						continue;
					}

					auto msgSize = measure(span.message, width - (startCol + 2));

					int spaceBetween;
					foreach (v; messageLines[startCol + 2 .. startCol + 2 + msgSize[1]]) {
						spaceBetween = max(spaceBetween, v);
					}

					atRow = squigglyRow + spaceBetween + 1;
					foreach (j; squigglyRow + 1 .. atRow) {
						at(j, startCol) = Char(Art.L, squigglyColor);
					}
					at(atRow, startCol) = Char(Art.BL, squigglyColor);
					foreach (ref v; messageLines[startCol .. startCol + 2 + msgSize[1]]) {
						v = max(v, msgSize[0] + spaceBetween);
					}
					auto msgEnd = put(span.message, atRow, startCol + 2, squigglyColor, Color.DEFAULT);
				}
				if (!oneSpan) {
					atRow = squigglyRow;
					foreach (v; messageLines) {
						atRow = max(atRow, squigglyRow + v + 1);
					}
				}
			}

			atRow += PadBottom;

			foreach (row; gutterStartRow + 1 .. atRow) {
				at(row, gutter) = Char(Art.L, color);
			}

			foreach (col; 0 .. gutter) {
				at(atRow, col) = Char(Art.B, color);
			}

			at(atRow, gutter) = Char(Art.BR, color);

			atRow += 1;
		}

		SpanReport[][Source] reports;
		Source[] sources;

		void addSpanReport(Span span, SpanReport.Kind kind, string message) {
			sources ~= span.source;
			SpanReport report = {
				kind: kind,
				start: span.start,
				end: span.end,
				message: message,
			};
			if (span.source !in reports) {
				reports[span.source] = [];
			}
			reports[span.source] ~= report;
		}

		if (!span.isNull) {
			addSpanReport(span.get, SpanReport.Kind.It, inlineMessage);
		}

		foreach (e; extra) {
			if (!e.span.isNull) {
				SpanReport.Kind ekind;
				final switch (e.kind) {
				case Extra.Kind.Help:
					ekind = SpanReport.Kind.Help;
					break;
				case Extra.Kind.Note:
					ekind = SpanReport.Kind.Note;
					break;
				}
				addSpanReport(e.span.get, ekind, e.message);
			}
		}

		foreach (source; sources) {
			if (source in reports) {
				reportSpans(source, reports[source]);
				reports.remove(source);
			}
		}

		foreach (e; extra) {
			if (e.span.isNull) {
				Color extraColor;
				final switch (e.kind) {
				case Extra.Kind.Help:
					extraColor = Color.blue;
					break;
				case Extra.Kind.Note:
					extraColor = Color.white;
					break;
				}
				at(atRow, 4) = Char(Art.Bullet, bright(extraColor), Color.DEFAULT);
				auto afterKind = put(
					e.kind.to!string.replaceAll(ctRegex!r"([A-Z])", " $1")[1 .. $] ~ ": ", atRow, 6, bright(extraColor), Color.DEFAULT,
				);
				auto afterExtraMsg = put(
					e.message, afterKind[0], afterKind[1], Color.DEFAULT, Color.DEFAULT,
				);
				atRow = afterExtraMsg[0] + 1;
			}
		}

		Color currFg = cast(Color) -1;
		Color currBg = cast(Color) -1;
		foreach (i, row; contents) {
			int atCol = 0;
			foreach (j, col; row) {
				if (currFg != col.fg || currBg != col.bg) {
					currFg = col.fg;
					currBg = col.bg;
					term.color(currFg, currBg);
				}
				if (col.c != 0 && col.c != '\n') {
					while (atCol < j) {
						term.write(' ');
						atCol += 1;
					}
					term.write(col.c);
					atCol += 1;
				}
			}
			term.writeln();
		}

		// TODO: good non-tty error reporting
	}
}
