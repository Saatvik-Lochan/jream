{module, merge}.  %% version = 0

{exports, [{merge_sort,1},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 20}.


{function, merge_sort, 1, 2}.
  {label,1}.
    {line,[{location,"merge.erl",5}]}.
    {func_info,{atom,merge},{atom,merge_sort},1}.
  {label,2}.
    {line,[{location,"merge.erl",6}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {test,is_ge,
          {f,3},
          [{integer,1},{tr,{x,1},{t_integer,{0,288230376151711743}}}]}.
    return.
  {label,3}.
    {allocate,1,1}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge.erl",9}]}.
    {call,1,{f,8}}. % split/1
    {move,{x,0},{y,0}}.
    {test,is_tuple,{f,6},[{x,0}]}.
    {test,test_arity,{f,6},[{x,0},2]}.
    {test_heap,{alloc,[{words,2},{floats,0},{funs,1}]},0}.
    {bif,self,{f,0},[],{x,0}}.
    {get_tuple_element,{y,0},0,{x,1}}.
    {make_fun3,{f,19},0,0,{x,0},{list,[{x,1},{x,0}]}}.
    {line,[{location,"merge.erl",11}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {get_tuple_element,{y,0},1,{x,0}}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge.erl",12}]}.
    {call,1,{f,2}}. % merge_sort/1
    {move,{x,0},{y,0}}.
  {label,4}.
    {loop_rec,{f,5},{x,0}}.
    remove_message.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,10},1}. % merge/2
  {label,5}.
    {wait,{f,4}}.
  {label,6}.
    {line,[{location,"merge.erl",9}]}.
    {badmatch,{y,0}}.


{function, split, 1, 8}.
  {label,7}.
    {line,[{location,"merge.erl",19}]}.
    {func_info,{atom,merge},{atom,split},1}.
  {label,8}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {line,[{location,"merge.erl",20}]}.
    {gc_bif,length,{f,0},1,[{tr,{x,0},{t_list,any,nil}}],{x,1}}.
    {gc_bif,'div',
            {f,0},
            2,
            [{tr,{x,1},{t_integer,{0,288230376151711743}}},{integer,2}],
            {x,1}}.
    {swap,{x,0},{x,1}}.
    {call_ext_only,2,{extfunc,lists,split,2}}.


{function, merge, 2, 10}.
  {label,9}.
    {line,[{location,"merge.erl",23}]}.
    {func_info,{atom,merge},{atom,merge},2}.
  {label,10}.
    {test,is_nil,{f,11},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,11}.
    {test,is_nil,{f,12},[{x,1}]}.
    return.
  {label,12}.
    {test,is_nonempty_list,{f,9},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_nonempty_list,{f,9},[{x,1}]}.
    {get_list,{x,1},{x,4},{x,5}}.
    {test,is_ge,{f,13},[{x,4},{x,2}]}.
    {allocate,1,4}.
    {move,{x,2},{y,0}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"merge.erl",27}]}.
    {call,2,{f,10}}. % merge/2
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,13}.
    {allocate,1,6}.
    {move,{x,4},{y,0}}.
    {move,{x,5},{x,1}}.
    {line,[{location,"merge.erl",28}]}.
    {call,2,{f,10}}. % merge/2
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,any},
                                   {t_cons,any,any},
                                   {t_number,any},
                                   {t_tuple,0,false,#{}},
                                   other}}]}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, module_info, 0, 15}.
  {label,14}.
    {line,[]}.
    {func_info,{atom,merge},{atom,module_info},0}.
  {label,15}.
    {move,{atom,merge},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 17}.
  {label,16}.
    {line,[]}.
    {func_info,{atom,merge},{atom,module_info},1}.
  {label,17}.
    {move,{x,0},{x,1}}.
    {move,{atom,merge},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-merge_sort/1-fun-0-', 2, 19}.
  {label,18}.
    {line,[{location,"merge.erl",11}]}.
    {func_info,{atom,merge},{atom,'-merge_sort/1-fun-0-'},2}.
  {label,19}.
    {'%',{var_info,{x,1},[{type,pid}]}}.
    {allocate,1,2}.
    {move,{x,1},{y,0}}.
    {call,1,{f,2}}. % merge_sort/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    send.
    {deallocate,1}.
    return.
