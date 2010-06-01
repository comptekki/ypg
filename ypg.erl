%%%-------------------------------------------------------------------
%%% Created :  by comptekki May 20, 2010 (Original by Torbjorn Tornkvist <tobbe@tornkvist.org>)
%%% Desc.   : A postgresql table viewer Yaws-app interface.
%%%
%%% @author comptekki
%%%
%%% @doc ypg is a Yaws appmod to view/search postgresql tables.
%%%      Add <b>ypg</b> as an appmod to your Yaws configuration.
%%%      Point your browser to: &lt;url&gt;/ypg
%%%
%%%      <ul>
%%%      <li> You can search on any text.</li>
%%%      <li> The checkbox control if the attribute should be shown in
%%%           the result. <br/>No checkbox means: <i>show all attributes</i>.</li>
%%%      </ul>
%%%
%%%      <p>
%%%      To test it, add it as an appmod to you Yaws configuration, e.g:
%%%
%%%          appmods = <ypg, ypg>
%%%
%%%      then point your browser to:  http://hostname/ypg/
%%%      </p>
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(ypg).

-export([out/1]).

-import(lists, [map/2, foldl/3, reverse/1]).

-include("/usr/local/lib/yaws/include/yaws_api.hrl"). 
-include("ypg.hrl").


-define(elog(X,Y), error_logger:info_msg("*elog ~p:~p: " X,
                                         [?MODULE, ?LINE | Y])).

% http://localhost/p/?tablename=page_count&count=&date=&time=

%select tablename from pg_tables where schemaname='public'
%select column_name from information_schema.columns where table_name ='page_count'

%%% @private
out(A) ->
	case has_query(A) of
		true ->
			L = yaws_api:parse_query(A),
			{Cbox, Rest0} = extract_cbox(L),
			Offset=lk("offset", Rest0),
			{_, Rest1} = extract_offset(Rest0),
			Rpp=lk("rpp", Rest1),
			{_, Rest} = extract_rpp(Rest1),
			Name = lk("tablename", Rest),
			Ls = select_fields(Rest),
			Sp = (catch select_pattern(Name, Ls)),
			SpOffset = Sp ++ " offset " ++ Offset ++ " limit " ++ Rpp,
			table(Cbox, Sp, SpOffset, l2a(Name), list_to_integer(Rpp));
		false ->
		   return_top_page()
	end.

has_query(A) ->
	case length(yaws_api:parse_query(A)) of
        0  -> false;
        _ -> true
	end.
    
%%% Get the fields to be part of the select
select_fields([{"tablename",_}|T]) -> select_fields(T);
select_fields([{_,undefined}|T])   -> select_fields(T);
select_fields([H|T])               -> [H|select_fields(T)];
select_fields([])                  -> [].


select_pattern(Name, []) ->
	"select * from " ++ Name;
	
select_pattern(Name, Ls) ->
io:format("~nName: ~p~nLs: ~p~n", [Name,Ls]),
		S=lists:flatten([" and " ++ Field ++ "::text like '%" ++ Value ++ "%'" || {Field, Value} <- Ls]),
		S2=fun(" and " ++ Rest) -> Rest end,
	"select * from " ++ Name ++ " where " ++ S2(S).

return_top_page() ->
    {ehtml,
     [{head, [],
       [meta() ++
        style() ++
        js()
        ]},
      {body, [],
       mk_table_tab(10,0)}]}.

meta() ->
    [{pre_html, 
      "<META HTTP-EQUIV=\"EXPIRES\" CONTENT=\""
      "Sun, 16 Oct 2004 11:12:01 GMT\">"}].

style() ->
    [{style, [{type, "text/css"}],
      [{pre_html,
        ["\nbody {background-color:black;}\n",
         "table {border-collapse: collapse; border: solid black 1px; background-color:grey;}\n"
         "p {padding: 5px; font-weight: bold;}\n"
         "input[type=text] {vertical-align: bottom; width: 100%; font-size: 80%;}\n"
         "input[type=checkbox] {vertical-align: top; font-size: 80%;}\n"
         "span.attribute {vertical-align: top; font-size: 80%;}\n"
         "th {padding: 5px; border: solid black 1px;}\n"
         "td {padding: 5px; border: solid black 1px;}\n"
        ]}]}].

js() ->
	[{script, [{type, "text/javascript"}, {src, ?JQUERY}], ""},
	{script, [{type, "text/javascript"}], "$(document).ready(function() {$(':input:text:first').focus()})"}].
		
js2(Table) ->
		{script,
			[{type, "text/javascript"}],
				"$(document).ready(
					function() {
						$('#" ++ a2l(Table) ++"').click(function() {" ++
							  "
							$('#range_input').val(10);
							$('#offset').val(0)
							  $.ajax({
								   url: '/p/',
								   type: 'GET',
								   data: 'tablename=" ++ a2l(Table) ++ setfields(Table) ++ ",
								   success: function(data) {
									   $('#data').html(data) 
								   },
								   error:function(XMLHttpRequest, textStatus, errorThrown) {
									alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
								   }
						});
						$('#" ++ a2l(Table) ++ "_id').focus()
				 	});
				});"
		}.				

setfields(Table) ->
	"' + '&rpp=' + $('#range_input').val() + "++ " '&offset=' + $('#offset').val() + " ++
    map(
    	fun(Col) -> 
    		" '&"++a2l(Col)++"=' + $('#"++a2l(Col)++"').val() + ($('#cbox_"++a2l(Col)++"').attr('checked')?'&cbox_"++a2l(Col)++"='+ $('#cbox_"++a2l(Col)++"').val():'') + "
    	end,
    	get_columns(Table)) ++ "''".
	
js3(Table) ->
		{script,
			[{type, "text/javascript"}],
			%%
			map(
				fun(Col) ->
					"
					
					$(document).ready(
						function() {
							$('#" ++ a2l(Col) ++ "').keyup(function() {" ++
							  "$.ajax({
								  url: '/p/',
								  type: 'GET',
								  data: 'tablename=" ++ a2l(Table) ++ setfields(Table) ++ ",
								  success: function(data) {
									  $('#data').html(data) 
								  },
								  error:function(XMLHttpRequest, textStatus, errorThrown) {
								   alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
								  }
							});				
						});
					});
					
					$(document).ready(
						function() {
							$('#cbox_" ++ a2l(Col) ++ "').click(function() {
							  $.ajax({
								  url: '/p/',
								  type: 'GET',
								  data: 'tablename=" ++ a2l(Table) ++ setfields(Table) ++ ",
								  success: function(data) {
									  $('#data').html(data) 
								  },
								  error:function(XMLHttpRequest, textStatus, errorThrown) {
								   alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
								  }
							});				
						});
					});
					
					"
				end,
				get_columns(Table)
				)
			%%
		}.         
%%% Build the result page.
table(Cbox, Sp, SpOffset, Table, RowsPerPage) ->
        {Q, Result} = do_query(SpOffset),
        {_, Res2} = do_query(Sp),
		Count=length(Res2),
		case Count==0 of
			true ->
				{ehtml, {table, [], {tr, [], {td, [], "No Data"}}}};
			_ ->
				Headers=get_columns(Table),
				Vp = view_pattern(Cbox, map(fun(X) -> a2l(X) end, Headers)),
				Nav={table, [],
						{tr, [],
							mk_nav(Count, RowsPerPage, Table)
						}
					},
					{ehtml,
					   [{'div', [],
							[
							{br},
							{table, [],
								[
								{tr, [],
									[{td, [],
										{input, [{id,"range_input_view"},{type,"range"}, {name, "range_input_view"}, {min,"10"}, {max,"100"}, {value,RowsPerPage}, {step,"5"}]}
						
										},
									{td, [{style,"background-color:#cccccc;"}],
										{span, [],
											["Show ",
											{span, [{id,"range_val"}], "10"},
											" Rows"
											]
										}
									}
									]
								}]
							  },
							  {script,
								  [{type, "text/javascript"}],
									"$(document).ready(function() {											
										$('#range_val').html($('#range_input_view').val());
										$('#range_input_view').change(
											function() {
												$('#range_val').html($('#range_input_view').val());
												$('#range_input').val($('#range_input_view').val());
												$('#offset').val(0);
												$(':input:text:first').focus()
											});
										$('#range_input_view').click().mouseup(
											function() {
												$.ajax({
													  url: '/p/',
													  type: 'GET',
													  data: 'tablename=" ++ a2l(Table) ++ setfields(Table) ++ ",
													  success: function(data) {
														  $('#data').html(data) 
													  },
													  error:function(XMLHttpRequest, textStatus, errorThrown) {
													   alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
													  }
												});
											})
									});"
							  },
							{table, [],
								{tr, [],
									{td, [],
										[
											{p, [], "Query: "++Q},
											{p, [], "Records Found: "++io_lib:format("~p",[Count])}
										]}
									
								}
							}
						]},
					   {'div', [],
							[
								{table, [], {tr, [], {td, [], {p, [], "Table: "++a2l(Table)}}}},
								Nav
							]
					   } |
					   [mk_tab(Vp, Headers, Result),
					   Nav]
					   ]}
		end.	

mk_nav(Count, RowsPerPage, Table) ->
	Ni=Count div RowsPerPage,
	Nd=Count rem RowsPerPage,
	
	case Nd > 0 of
		true -> Nii = Ni + 1;
		_ -> Nii = Ni
	end,
	case Nii > 10 of
		true -> NavL = 10;
		_ -> NavL = Nii
	end,
	build_nav(1, NavL, RowsPerPage, Table).

build_nav(Start, End, RowsPerPage, Table) ->

	case Start==End of
		false -> 
			[{td, [], {a, [{href, "javascript:void(0);"}, {id, Start},
				{onclick, "$('#offset').val(" ++ io_lib:format("~p",[(Start-1)*RowsPerPage]) ++");
			$.ajax({
				 url: '/p/',
				 type: 'GET',
				 data: 'tablename=" ++ a2l(Table) ++ setfields(Table) ++ ",
				 success: function(data) {
					 $('#data').html(data) 
				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });$(':input:text:first').focus();
			
			"}], [io_lib:format("~p",[Start])]}} | build_nav(Start+1,End, RowsPerPage, Table)];
		_ -> {td, [], {a, [{href, "javascript:void(0);"}, {id, Start}, 
			{onclick, "$('#offset').val(" ++ io_lib:format("~p",[(Start-1)*RowsPerPage]) ++");
		
			$.ajax({
				 url: '/p/',
				 type: 'GET',
				 data: 'tablename=" ++ a2l(Table) ++ setfields(Table) ++ ",
				 success: function(data) {
					 $('#data').html(data) 
				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });	$(':input:text:first').focus();	
		
		"}], [io_lib:format("~p",[Start])]}}
	end.
	
%%% Create a pattern denoting which fields to show in the result.
view_pattern(Cs, L) -> view_pattern(Cs, L, 1).

view_pattern([Cbox|Cs], [Cbox|T], N) -> [N | view_pattern(Cs, T, N+1)];
view_pattern(Cs, [_|T], N)           -> view_pattern(Cs, T, N+1);
view_pattern([], [], _)              -> [].

do_query(Sp) ->
	{ok, Db} = pgsql:connect(?HOST, ?DB, ?USERNAME, ?PASSWORD),
	{_,[{_,_,Res}]}=pgsql:squery(Db, Sp),
	pgsql:terminate(Db),
	{Sp, Res}.
	
%%% Create a table of: Table | Table-attribute-1 | ... | Table-attribute-N
%%% where each table is a Form

mk_table_tab(RowsPerPage, Offset) ->
	Tables=get_tables(),
    [{input, [{id, "range_input"}, {type, "hidden"}, {value, RowsPerPage}]}, {input, [{id, "offset"}, {type, "hidden"}, {value, Offset}]},
    
    {'div', [],
      {table, [],
        map(fun(Table) ->
					Tab=Table,
                    {tr, [], 
                     [
                    js2(Table),
                    js3(Table),
                      {td, [], sublnk(Tab)} |
                      	mk_input_fields(Tab)
                    
                    ]
                   }
            end, Tables)}
       },
     {'div', [{id, "data"}]}
    ].

%%% Create each table cell; consisting of the attribute name and an input field.
mk_input_fields(Table) ->
    As = get_columns(Table),
    Max = max_noof_attrs(),
    map(fun(0) ->
                {td, [], []};
           (Attribute) ->
                A = a2l(Attribute),
                {td, [], 
                 [{input, [{id,"cbox_"++A}, {type, "checkbox"}, {name, "cbox_"++A}]},
                  {span, [{class, "attribute"}], A}, 
                  {input, [{id, A}, {type, "text"}, {name, A}]}]}
        end, As ++ lists:duplicate(Max-length(As), 0)).


extract_cbox(L) ->
    extract_cbox(L, [], []).

extract_cbox([{"cbox_"++Cbox,_}|T], Cs, Rs) ->
    extract_cbox(T, [Cbox|Cs], Rs);
extract_cbox([H|T], Cs, Rs) ->
    extract_cbox(T, Cs, [H|Rs]);
extract_cbox([], Cs, Rs) ->
    {reverse(Cs), reverse(Rs)}.


extract_rpp(L) ->
    extract_rpp(L, [], []).

extract_rpp([{"rpp"++Rpp,_}|T], Cs, Rs) ->
    extract_rpp(T, [Rpp|Cs], Rs);
extract_rpp([H|T], Cs, Rs) ->
    extract_rpp(T, Cs, [H|Rs]);
extract_rpp([], Cs, Rs) ->
    {reverse(Cs), reverse(Rs)}.


extract_offset(L) ->
    extract_offset(L, [], []).

extract_offset([{"offset"++Offset,_}|T], Cs, Rs) ->
    extract_offset(T, [Offset|Cs], Rs);
extract_offset([H|T], Cs, Rs) ->
    extract_offset(T, Cs, [H|Rs]);
extract_offset([], Cs, Rs) ->
    {reverse(Cs), reverse(Rs)}.
    
%%% Build the result table.
mk_tab(Vp, Headers, Rows) ->
    [{'div', [],
      [{table, [],
        [
        {tr, [],
          [{th, [], a2l(X)} || X <- vp(Vp,Headers)]} |
         map(fun(Row) ->
                     {tr, [],
                      [{td, [], massage(W)} || W <- vp(Vp,Row)
                      ]}
             end, Rows)
        ]
      }]
    }].
	
%%% Match the view pattern to select which entries to let through.
vp([], L) -> L;
vp(Vp, L) -> vp(Vp, L, 1).

vp([N|Vp], [H|T], N) -> [H|vp(Vp, T, N+1)];
vp(Vp, [_|T], N)     -> vp(Vp, T, N+1);
vp([], [], _)        -> [].

%%% Create a link that submit the form: onclick
sublnk(E) -> 
    {a, [{href, "javascript:void(0);"}, {id, E}],
     [E]}.
               
massage(W) ->
    lists:flatten(io_lib:format("~p",[W])).
   
get_tables() ->
	{ok, Db} = pgsql:connect(?HOST, ?DB, ?USERNAME, ?PASSWORD),
	{_,[{_,_,Res}]}=pgsql:squery(Db, "select tablename from pg_tables where schemaname='public'"),
	pgsql:terminate(Db),
	Res.

get_columns(Table) ->
	{ok, Db} = pgsql:connect(?HOST, ?DB, ?USERNAME, ?PASSWORD),
	{_,[{_,_,Res}]}=pgsql:squery(Db, "select column_name from information_schema.columns where table_name ='" ++ a2l(Table) ++ "'"),
	pgsql:terminate(Db),
	lists:map(fun([Col]) -> l2a(Col) end, Res).


max_noof_attrs() ->
    foldl(fun(Table, Max) ->
                  max(length(get_columns(Table)), Max)
          end, 0, get_tables()).

max(X,Y) when X>Y   -> X;
max(X,Y) when X=< Y -> Y.


a2l(A) when atom(A) -> atom_to_list(A);
a2l(L) when list(L) -> L.

l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.

lk(Key, L) ->
    {value, {_,Val}} = lists:keysearch(Key, 1, L),
    Val.