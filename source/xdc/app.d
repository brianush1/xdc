import xdc.source;
import xdc.parser;
import std.conv : to;

Diagnostic[] main_(string[] args) {
	import std.stdio : writeln, File;
	import std.algorithm : map, joiner;

	auto f = File("source/arsd/terminal.d", "rb");
	scope (exit)
		f.close();
	string source = f.readln('\0'); // not the right way to do this, but it's temporary code anyway
	Source src = new Source("arsd/terminal.d", source);
	Lexer lexer = new Lexer(src);
	writeln(lexer.map!(x => x.to!string).joiner("\n"));

	return lexer.diagnostics;
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

		diagnostics ~= main_(args);

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
			Nullable!Span.init, e.to!string, "", 0, []
		);
		return 2;
	}
}
