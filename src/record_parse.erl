%%%-------------------------------------------------------------------
%%% @author Renaud Mariana <rmariana@gmail.com>
%%% @copyright (C) 2017, k
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2017 by Renaud Mariana <rmariana@gmail.com>
%%% Based on work of synrc/rest project
%%%-------------------------------------------------------------------
-module(record_parse).

-export([parse_transform/2,
	 generate_from_json/2, generate_from_json2/2, generate_from_json1/2, generate_from_json_field/2,
	 generate_to_json/2, generate_to_json1/2, generate_to_json2/2,
	 from_json/1, from_json/2, to_json/1, skip_undefined/1, skip_list/1, to_binary/1
	]).

parse_transform(Forms, _Options) ->
    RecordsFields = parse_record(Forms,[]),
    %% ct:print("parse_transform IN ~p Forms:~p", [RecordsFields, Forms ]),
    Forms1 = generate({from_json, 1}, export, RecordsFields, Forms),
    Forms2 = generate({from_json1, 1}, noexport, RecordsFields, Forms1),
    Forms3 = generate({from_json2, 3}, noexport, RecordsFields, Forms2),
    Forms4 = generate({from_json_field, 1}, noexport, RecordsFields, Forms3),
    Forms5 = generate({to_json, 1}, export, RecordsFields, Forms4),
    Forms6 = generate({to_json1, 1}, noexport, RecordsFields, Forms5),
    Forms7 = generate({to_json2, 1}, noexport, RecordsFields, Forms6),
    %% io:fwrite("----------~n~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms7),[{paper, 140},{ribbon, 100}])]),
    Forms7.

parse_record([], Acc) ->
    Acc;
parse_record([{attribute, _, record, {RecordName, Fields}} | Forms], Acc) ->
    Fs=  [record_field(Field) || Field <- Fields],
    parse_record(Forms, [{RecordName, Fs}|Acc]);
parse_record([_ | Forms], Acc) ->
    parse_record(Forms, Acc).

record_field({record_field, _, {atom, _, Field}   }) ->   {nil, Field};
record_field({record_field, _, {atom, _, Field}, {atom, _, _}}) -> {atom, Field};
record_field({record_field, _, {atom, _, Field}, _}) -> {nil, Field};
record_field({typed_record_field, {record_field,_,{atom, _, Field}}, {type,_,atom,_}}) -> {atom, Field};
record_field({typed_record_field, {record_field,_,{atom, _, Field}}, _}) -> {nil, Field};
record_field({typed_record_field, {record_field,_,{atom, _, Field},_}, {type,_,atom,_}}) -> {atom, Field};
record_field({typed_record_field, {record_field,_,{atom, _, Field},_}, _}) -> {nil, Field}.

last_export_line(Exports) ->
    case lists:reverse(Exports) of
         [{_, Line, _, _} | _] -> Line;
         _ -> 0 end.

export({attribute, LastExportLine, export, Exports}, Fun, LastExportLine, export) ->
    {attribute, LastExportLine, export, [Fun | Exports]};
export(Form, _, _, _) -> Form.

generate({FunName, _Arity} = Fun, Exported, RecordsFields, Forms) ->
    Exports = lists:filter(fun({attribute, _, export, _}) -> true; (_) -> false end, Forms),
    case exported(Fun, Exports) of
        true  -> Forms;
        false ->
            Line = last_export_line(Exports),
           Gen = list_to_atom("generate_" ++ atom_to_list(FunName)),
            lists:flatten([?MODULE:Gen(export(Form, Fun, Line, Exported), RecordsFields) || Form <- Forms])
    end.

exported(Fun, Exports) -> lists:member(Fun, lists:flatten([E || {attribute, _, export, E} <- Exports])).

field_var(Field) ->
    list_to_atom("V_" ++ atom_to_list(element(2,Field))).

generate_from_json_field({eof, Line}, _) ->
    [{function,Line,from_json_field,2,
       [{clause,Line,
	 [{var, Line, 'T'},
	  {match,Line,
	   {cons,Line,
	    {var,Line,'_'},
	    {cons,Line,{var,Line,'_'},{var,Line,'_'}}},
	   {var,Line,'Es'}}],
	 [],
	 [{lc,Line,
	   {call,Line,
	    {remote,Line,
	     {atom,Line,?MODULE},
	     {atom,Line,skip_list}},
	    [{call,Line,
	      {atom,Line,from_json_field},
	      [{var,Line,'T'},{var,Line,'E'}]}]},
	   [{generate,Line,{var,Line,'E'},{var,Line,'Es'}}]}]},
	{clause,Line,[{var, Line, '_'},{nil,Line}],[],[{nil,Line}]},
	{clause,Line,
	 [{var, Line, '_'},
	  {cons,Line,
	   {match,Line,
	    {tuple,Line,
	     [{bin,Line,
	       [{bin_element,Line,
		 {string,Line,"rzd_"},
		 default,default},
		{bin_element,Line,
		 {var,Line,'_'},
		 default,
		 [binary]}]},
	      {var,Line,'_'}]},
	    {var,Line,'F'}},
	   {nil,Line}}],
	 [],
	 [{call,Line,{atom,Line,from_json1},[{var,Line,'F'}]}]},
	{clause,Line,
	 [{var, Line, 'T'},
	  {cons,Line,
	   {match,Line,
	    {tuple,Line,
	     [{bin,Line,
	       [{bin_element,Line,
		 {string,Line,"rzd_"},
		 default,default},
		{bin_element,Line,
		 {var,Line,'_'},
		 default,
		 [binary]}]},
	      {var,Line,'_'}]},
	    {var,Line,'F'}},
	   {var,Line,'Rest'}}],
	 [],
	 [{cons,Line,
	   {call,Line,{atom,Line,from_json1},[{var,Line,'F'}]},
	   {call,Line,
	    {atom,Line,from_json_field},
	    [{var,Line,'T'},{var,Line,'Rest'}]}}]},
	{clause,Line,
	 [{var, Line, '_'},
	  {match,Line, 
	   {tuple,Line,
	    [{bin,Line,
	      [{bin_element,Line,
		{string,Line,"rzd_"},
		default,default},
	       {bin_element,Line,
		{var,Line,'_'},
		default,
		[binary]}]},
	     {var,Line,'_'}]},
	   {var,Line,'F'}}],
	 [],
	 [{call,Line,{atom,Line,from_json},[{var,Line,'F'}]}]},
	{clause,Line,
	 [{var, Line, 'Type'},{var,Line,'F'}],
	 [],
	 [{call,Line,
	   {remote,Line,
	    {atom,Line,?MODULE},
	    {atom,Line,from_json}},
	   [{var, Line, 'Type'},
	    {var,Line,'F'}]}]}]},

      {eof, Line + 1}];
generate_from_json_field(Form, _) -> Form.

generate_from_json({eof, Line}, _) ->
    [ {function,Line,from_json,1,
       [{clause,Line,
	 [{match,Line,
	   {cons,Line,
	    {var,Line,'_'},
	    {cons,Line,{var,Line,'_'},{var,Line,'_'}}},
	   {var,Line,'Es'}}],
	 [],
	 [{lc,Line,
	   {call,Line,{atom,Line,from_json1},[{var,Line,'E'}]},
	   [{generate,Line,{var,Line,'E'},{var,Line,'Es'}}]}]},
	{clause,Line,
	 [{var,Line,'E'}],
	 [],
	 [{call,Line,
	   {atom,Line,from_json1},
	   [{var,Line,'E'}]}]}]},
      {eof, Line + 1}];
generate_from_json(Form, _) -> Form.

generate_from_json1({eof, Line}, RecordsFields) ->
    [{function, Line, from_json1, 1, [from_json1_prelude(Line)] ++ lists:flatten(
      [from_json1_clauses(Line, Record) || {Record, _Fields} <- RecordsFields]) ++ [from_json1_coda(Line)] },
     {eof, Line + 1}];
generate_from_json1(Form, _) -> Form.

from_json1_prelude(Line) ->
    {clause,Line,
     [{cons,Line,
       {tuple,Line,
	[{match,Line,
	  {bin,Line,
	   [{bin_element,Line,
	     {string,Line,"rzd_"},
	     default,default},
	    {bin_element,Line,
	     {var,Line,'_'},
	     default,
	     [binary]}]},
	  {var,Line,'R'}},
	 {var,Line,'Fields'}]},
       {nil,Line}}],
     [],
     [{call,Line,
       {atom,Line,from_json1},
       [{tuple,Line,
	 [{var,Line,'R'},{var,Line,'Fields'}]}]}]}.

from_json1_clauses(Line, Record) ->
    {clause, Line,
     [{tuple,Line,
       [{bin,Line,
	 [{bin_element,Line,
	   {string,Line,"rzd_"++ atom_to_list(Record)},
	   default,default}]},
	{var,Line,'Fields'}]}],
     [],
     [{call,Line,
       {atom,Line,from_json2},
       [{bin,Line,
	 [{bin_element,Line,
	   {string,Line, "rzd_"++ atom_to_list(Record)},
	   default,default}]},
	{var,Line,'Fields'},
	{record,Line, Record,[]}]}
     ]}.
from_json1_coda(Line) ->
    {clause, Line,
     [{var,Line,'J'}],
     [],
     [{call,Line,
       {remote,Line,
	{atom,Line,?MODULE},
	{atom,Line,from_json}},
       [{var,Line,'J'}]}]}.

generate_from_json2({eof, Line}, RecordsFields) ->
    [{function, Line, from_json2, 3, [from_json2_prelude(Line)] ++ lists:flatten(
	  from_json2_records_clauses(Line, RecordsFields)) },
     {eof, Line + 1}];
generate_from_json2(Form, _) -> Form.

from_json2_records_clauses(Line, RecordsFields) ->
    [from_json2_clauses(Line, Record, Fields) ++ [from_json2_coda(Line, Record)] || {Record, Fields} <- RecordsFields].

from_json2_prelude(Line) ->
    {clause, Line,
     [{var, Line, '_'},
      {nil, Line}, {var, Line, 'Acc'}],
     [],
     [{var, Line, 'Acc'}]}.

from_json2_coda(Line, Record) ->
    {clause, Line,
     [{bin,Line,
       [{bin_element,Line,
	 {string,Line,"rzd_"++atom_to_list(Record)},
	 default,default}]},
      {cons,Line,{var,Line,'_'},{var,Line,'Json'}},
      {var,Line,'Acc'}],
      [],
     [{call,Line,
       {atom,Line,from_json2},
       [{bin,Line,
	 [{bin_element,Line,
	   {string,Line,"rzd_"++atom_to_list(Record)},
	   default,default}]},
	{var,Line,'Json'},
	{var,Line,'Acc'}]}]
    }.

from_json2_clauses(_, _, []) -> [];
from_json2_clauses(Line, Record, [Field | Fields]) ->
    [{clause, Line,
      [{bin,Line,
	[{bin_element,Line,
	  {string,Line,"rzd_"++atom_to_list(Record)},
	  default,default}]},
       {cons,Line,
	{tuple,Line,
	 [{bin,Line,
	   [{bin_element,Line,
	     {string,Line, atom_to_list(element(2,Field))},
	     default,default}]},
	  {var,Line,field_var(Field)}]},
	{var,Line,'Json'}},
       {var,Line,'Acc'}],
      [],
      [{call,Line,
	{atom,Line,from_json2},
	[{bin,Line,
	  [{bin_element,Line,
	    {string,Line,"rzd_"++ atom_to_list(Record)},
	    default,default}]},
	 {var,Line,'Json'},
	 {record,Line,
	  {var,Line,'Acc'},
	  Record,
	  [{record_field,Line,
	    {atom,Line,element(2,Field)},
	    {call,Line,
	     {atom,Line,from_json_field},
	     [{atom,Line,element(1,Field)},
	      {var,Line,field_var(Field)}]

	    }}]}]}
      ]}
     | from_json2_clauses(Line, Record, Fields)].

generate_to_json({eof, Line}, _) ->
    [{function,Line,to_json,1,
      [{clause,Line,
	[{match,Line,
	  {cons,Line,
	   {var,Line,'_'},
	   {cons,Line,{var,Line,'_'},{var,Line,'_'}}},
	  {var,Line,'Es'}}],
	[],
	[{lc,Line,
	  {call,Line,{atom,Line,to_json1},[{var,Line,'E'}]},
	  [{generate,Line,{var,Line,'E'},{var,Line,'Es'}}]}]},
       {clause,Line,
	[{var,Line,'E'}],
	[],
	[{call,Line,{atom,Line,to_json1},[{var,Line,'E'}]}]}]},
     {eof, Line + 1}];
generate_to_json(Form, _) -> Form.


generate_to_json1({eof, Line}, _) ->
    [{function,Line,to_json1,1,
      [{clause,Line,
	[{var,Line,'R'}],
	[[{call,Line,{atom,Line,is_tuple},[{var,Line,'R'}]}]],
	[{call,Line,{atom,Line,to_json2},[{var,Line,'R'}]}]},
       {clause,Line,
	[{var,Line,'R'}],
	[],
	[{call,Line,
	  {remote,Line,
	   {atom,Line,?MODULE},
	   {atom,Line,to_json}},
	  [{var,Line,'R'}]}]}]},
     {eof, Line + 1}];
generate_to_json1(Form, _) -> Form.

generate_to_json2({eof, Line}, RecordsFields) ->
    [{function, Line, to_json2, 1, lists:flatten(
      [to_json2_clauses(Line, Record, Fields) || {Record, Fields} <- RecordsFields]) ++ [to_json2_coda(Line)]},
     {eof, Line + 1}];
generate_to_json2(Form, _) -> Form.

to_json2_clauses(Line, Record, Fields) ->
   {clause,Line,
     [{record, Line, Record,
       [{record_field, Line, {atom, Line, element(2,F)}, {var, Line, field_var(F)}} || F <- Fields]}],
     [],
     [{cons,Line,
       {tuple,Line,
    	[{bin,Line,
    	  [{bin_element,Line,
    	    {string,Line,"rzd_"++ atom_to_list(Record)},
    	    default,default}]},
    	 {call,Line,
    	  {remote,Line,
    	   {atom,Line,?MODULE},
    	   {atom,Line,skip_undefined}},
	 [to_json_cons(Line, Fields)]
	 }]},
       {nil,Line}}]}.

to_json_cons(Line, []) -> {nil, Line};
to_json_cons(Line, [Field | Fields]) ->
    {cons, Line,
     {tuple, Line,
      [{atom, Line, element(2,Field)},
       {call, Line,
	{atom, Line, to_json},
        [{var, Line, field_var(Field)}]}]},
     to_json_cons(Line, Fields)}.

to_json2_coda(Line) ->
    {clause,Line,
     [{var,Line,'X'}],
     [],
     [{call,Line,
       {remote,Line,
	{atom,Line,?MODULE},
	{atom,Line,to_json}},
       [{var,Line,'X'}]}]}.

from_json(Data) -> from_json(nil, Data).
    
from_json(atom, <<Data/binary>>) -> binary_to_atom(Data, latin1);
from_json(_, <<Data/binary>>) -> binary_to_list(Data);
from_json(_,{struct, Props}) -> from_json(Props);
from_json(_, [{Key, _} | _] = Props) when Key =/= struct -> lists:foldr(fun props_skip/2, [], Props);
from_json(_, [_|_] = NonEmptyList) -> [from_json(X) || X <- NonEmptyList];
from_json(_, Any) -> Any.

props_skip({<<BinaryKey/binary>>, Value}, Acc) ->
    try Key = list_to_existing_atom(binary_to_list(BinaryKey)),
        props_skip({Key, Value}, Acc)
    catch _:_ -> Acc end;
props_skip({Key, Value}, Acc) -> [{Key, from_json(Value)} | Acc].

to_json(Data) ->
    case is_string(Data) of
        true  -> ?MODULE:to_binary(Data);
        false -> json_match(Data)
    end.

json_match([{_, _} | _] = Props) -> [{?MODULE:to_binary(Key), to_json(Value)} || {Key, Value} <- Props];
json_match([_ | _] = NonEmptyList) -> [to_json(X) || X <- NonEmptyList];
json_match(Any) -> Any.

is_char(C) -> is_integer(C) andalso C >= 0 andalso C =< 255.

is_string([N | _] = PossibleString) when is_number(N) -> lists:all(fun is_char/1, PossibleString);
is_string(_)                                          -> false.

to_binary(A) when is_atom(A) -> atom_to_binary(A,latin1);
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(F) when is_float(F) -> float_to_binary(F,[{decimals,9},compact]);
to_binary(L) when is_list(L) ->  iolist_to_binary(L).

skip_undefined(Props) ->
    [{Key, V} || {Key, V} <- Props, V /= undefined].

skip_list([E]) -> E;
skip_list(E) -> E.
