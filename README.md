
record_parse: Erlang (nested) records with typed JSON
=====================================================

Feature
-------
typed JSON from erlang records is achived with parse_transform code generation for tuple transformations.

Usage
-----

Sample record_parse implementation:

```erlang
-module(test).
-export([encode_decode/0]).

-compile({parse_transform, record_parse}).

-record(blade, {id, ancestor=element, dep}).
-record(cpu, {id::atom()}).

encode_decode() ->
     E = #blade{id=123, dep=#cpu{id=gupil1}},
     E = ?MODULE:from_json(
	jsone:decode(jsone:encode(
		?MODULE:to_json(E)), [{object_format, proplist}])).

```
