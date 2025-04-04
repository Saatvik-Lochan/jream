{module, merge}.  %% version = 0

{exports, [{merge_sort,1},{module_info,0},{module_info,1},{woah,0}]}.

{attributes, []}.

{labels, 26}.


{function, temp, 0, 2}.
  {label,1}.
    {line,[{location,"merge.erl",4}]}.
    {func_info,{atom,merge},{atom,temp},0}.
  {label,2}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {line,[{location,"merge.erl",5}]}.
    {call_ext,0,{extfunc,rand,uint64_t,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"merge.erl",6}]}.
    {call_ext,0,{extfunc,rand,uint64_t,0}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"merge.erl",7}]}.
    {call_ext,0,{extfunc,rand,uint64_t,0}}.
    {test_heap,{alloc,[{words,3},{floats,0},{funs,1}]},1}.
    {make_fun3,{f,25},0,0,{x,0},{list,[{y,1},{y,0},{x,0}]}}.
    {'%',{var_info,{x,0},[{fun_type,{t_number,any}}]}}.
    {deallocate,2}.
    return.


{function, woah, 0, 4}.
  {label,3}.
    {line,[{location,"merge.erl",10}]}.
    {func_info,{atom,merge},{atom,woah},0}.
  {label,4}.
    {allocate,0,0}.
    {line,[{location,"merge.erl",11}]}.
    {call,0,{f,2}}. % temp/0
    {'%',{var_info,{x,0},
                   [{type,{t_fun,2,{'-temp/0-fun-0-',5},{t_number,any}}}]}}.
    {move,{integer,2},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"merge.erl",12}]}.
    {call_fun2,{f,25},
               2,
               {tr,{x,2},{t_fun,2,{'-temp/0-fun-0-',5},{t_number,any}}}}.
    {deallocate,0}.
    return.


{function, merge_sort, 1, 6}.
  {label,5}.
    {line,[{location,"merge.erl",15}]}.
    {func_info,{atom,merge},{atom,merge_sort},1}.
  {label,6}.
    {line,[{location,"merge.erl",16}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {test,is_ge,
          {f,7},
          [{integer,1},{tr,{x,1},{t_integer,{0,288230376151711743}}}]}.
    return.
  {label,7}.
    {allocate,1,1}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge.erl",19}]}.
    {call,1,{f,12}}. % split/1
    {move,{x,0},{y,0}}.
    {test,is_tuple,{f,10},[{x,0}]}.
    {test,test_arity,{f,10},[{x,0},2]}.
    {test_heap,{alloc,[{words,2},{floats,0},{funs,1}]},0}.
    {bif,self,{f,0},[],{x,0}}.
    {get_tuple_element,{y,0},0,{x,1}}.
    {make_fun3,{f,23},0,0,{x,0},{list,[{x,1},{x,0}]}}.
    {line,[{location,"merge.erl",21}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {get_tuple_element,{y,0},1,{x,0}}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge.erl",22}]}.
    {call,1,{f,6}}. % merge_sort/1
    {move,{x,0},{y,0}}.
  {label,8}.
    {loop_rec,{f,9},{x,0}}.
    remove_message.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,14},1}. % merge/2
  {label,9}.
    {wait,{f,8}}.
  {label,10}.
    {line,[{location,"merge.erl",19}]}.
    {badmatch,{y,0}}.


{function, split, 1, 12}.
  {label,11}.
    {line,[{location,"merge.erl",29}]}.
    {func_info,{atom,merge},{atom,split},1}.
  {label,12}.
    {'%',{var_info,{x,0},[{type,{t_list,any,nil}}]}}.
    {line,[{location,"merge.erl",30}]}.
    {gc_bif,length,{f,0},1,[{tr,{x,0},{t_list,any,nil}}],{x,1}}.
    {gc_bif,'div',
            {f,0},
            2,
            [{tr,{x,1},{t_integer,{0,288230376151711743}}},{integer,2}],
            {x,1}}.
    {swap,{x,0},{x,1}}.
    {call_ext_only,2,{extfunc,lists,split,2}}.


{function, merge, 2, 14}.
  {label,13}.
    {line,[{location,"merge.erl",33}]}.
    {func_info,{atom,merge},{atom,merge},2}.
  {label,14}.
    {test,is_nil,{f,15},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,15}.
    {test,is_nil,{f,16},[{x,1}]}.
    return.
  {label,16}.
    {test,is_nonempty_list,{f,13},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_nonempty_list,{f,13},[{x,1}]}.
    {get_list,{x,1},{x,4},{x,5}}.
    {test,is_ge,{f,17},[{x,4},{x,2}]}.
    {allocate,1,4}.
    {move,{x,2},{y,0}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"merge.erl",37}]}.
    {call,2,{f,14}}. % merge/2
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,17}.
    {allocate,1,6}.
    {move,{x,4},{y,0}}.
    {move,{x,5},{x,1}}.
    {line,[{location,"merge.erl",38}]}.
    {call,2,{f,14}}. % merge/2
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


{function, module_info, 0, 19}.
  {label,18}.
    {line,[]}.
    {func_info,{atom,merge},{atom,module_info},0}.
  {label,19}.
    {move,{atom,merge},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 21}.
  {label,20}.
    {line,[]}.
    {func_info,{atom,merge},{atom,module_info},1}.
  {label,21}.
    {move,{x,0},{x,1}}.
    {move,{atom,merge},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-merge_sort/1-fun-0-', 2, 23}.
  {label,22}.
    {line,[{location,"merge.erl",21}]}.
    {func_info,{atom,merge},{atom,'-merge_sort/1-fun-0-'},2}.
  {label,23}.
    {'%',{var_info,{x,1},[{type,pid}]}}.
    {allocate,1,2}.
    {move,{x,1},{y,0}}.
    {call,1,{f,6}}. % merge_sort/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    send.
    {deallocate,1}.
    return.


{function, '-temp/0-fun-0-', 5, 25}.
  {label,24}.
    {line,[{location,"merge.erl",8}]}.
    {func_info,{atom,merge},{atom,'-temp/0-fun-0-'},5}.
  {label,25}.
    {gc_bif,'+',{f,0},5,[{x,0},{x,1}],{x,0}}.
    {gc_bif,'-',{f,0},5,[{tr,{x,0},{t_number,any}},{x,2}],{x,0}}.
    {gc_bif,'+',{f,0},5,[{tr,{x,0},{t_number,any}},{x,3}],{x,0}}.
    {gc_bif,'-',{f,0},5,[{tr,{x,0},{t_number,any}},{x,4}],{x,0}}.
    return.
