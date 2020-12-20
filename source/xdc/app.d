import xdc.source;
import xdc.parser;
import std.conv : to;

void main_(ref Diagnostic[] diagnostics, string[] args) {
	import std.stdio : writeln, File;
	import std.algorithm : map, joiner;

	auto f = File("source/arsd/terminal.d", "rb");
	scope (exit)
		f.close();
	string source = f.readln('\0'); // not the right way to do this, but it's temporary code anyway
	Source src = new Source("arsd/terminal.d", source);
	Lexer lexer = Lexer(src, diagnostics);
	writeln(lexer.map!(x => x.to!string).joiner("\n"));
}

int main(string[] args) {
	import core.exception : AssertError;

	Diagnostic[] diagnostics;
	scope(exit) {
		foreach (diagnostic; diagnostics) {
			diagnostic.report();
		}
	}

	try {
		import std.algorithm : any;

		main_(diagnostics, args);

		if (diagnostics.any!(x => x.kind == Diagnostic.Kind.InternalError)) {
			return 2;
		}
		else if (diagnostics.any!(x => x.kind == Diagnostic.Kind.Error)) {
			return 1;
		}
		else {
			return 0;
		}
	}
	catch (AssertError e) {
		import std.typecons : Nullable;

		diagnostics ~= Diagnostic(Diagnostic.Kind.InternalError,
			Nullable!Span.init, e.file ~ ":" ~ e.line.to!string
				~ ":" ~ e.msg, "", 0, []
		);
		return 2;
	}
}
