{module, merge_sort}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{sort,1}]}.

{attributes, []}.

{labels, 18}.


{function, sort, 1, 2}.
  {label,1}.
    {line,[{location,"merge_sort.erl",4}]}.
    {func_info,{atom,merge_sort},{atom,sort},1}.
  {label,2}.
    {test,is_nonempty_list,{f,3},[{x,0}]}.
    {get_tl,{x,0},{x,1}}.
    {test,is_nil,{f,4},[{x,1}]}.
    return.
  {label,3}.
    {test,is_nil,{f,4},[{x,0}]}.
    return.
  {label,4}.
    {allocate,1,1}.
    {init_yregs,{list,[{y,0}]}}.
    {line,[{location,"merge_sort.erl",8}]}.
    {call,1,{f,6}}. % split/1
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,2,true,
                                   #{1 => {t_cons,any,any},
                                     2 => {t_list,any,nil}}}}]}}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"merge_sort.erl",11}]}.
    {call,1,{f,2}}. % sort/1
    {'%',{var_info,{x,0},
                   [{type,{t_union,{t_atom,any},
                                   {t_cons,any,any},
                                   {t_number,any},
                                   {t_tuple,0,false,#{}},
                                   other}}]}}.
    {swap,{y,0},{x,0}}.
    {line,[{location,"merge_sort.erl",12}]}.
    {call,1,{f,2}}. % sort/1
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {call_last,2,{f,10},1}. % merge/2


{function, split, 1, 6}.
  {label,5}.
    {line,[{location,"merge_sort.erl",18}]}.
    {func_info,{atom,merge_sort},{atom,split},1}.
  {label,6}.
    {test,is_nonempty_list,{f,8},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,7},[{x,2}]}.
    {allocate,2,3}.
    {move,{x,1},{y,1}}.
    {get_list,{x,2},{y,0},{x,0}}.
    {line,[{location,"merge_sort.erl",21}]}.
    {call,1,{f,6}}. % split/1
    {'%',{var_info,{x,0},
                   [{type,{t_tuple,2,true,
                                   #{1 => {t_list,any,any},
                                     2 => {t_list,any,nil}}}}]}}.
    {test_heap,7,1}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,0}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{y,0},{x,0},{x,0}}.
    {put_tuple2,{x,0},{list,[{x,1},{x,0}]}}.
    {deallocate,2}.
    return.
  {label,7}.
    {test,is_nil,{f,5},[{x,2}]}.
    {test_heap,3,1}.
    {put_tuple2,{x,0},{list,[{x,0},nil]}}.
    return.
  {label,8}.
    {test,is_nil,{f,5},[{x,0}]}.
    {move,{literal,{[],[]}},{x,0}}.
    return.


{function, merge, 2, 10}.
  {label,9}.
    {line,[{location,"merge_sort.erl",25}]}.
    {func_info,{atom,merge_sort},{atom,merge},2}.
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
    {line,[{location,"merge_sort.erl",28}]}.
    {call,2,{f,10}}. % merge/2
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,13}.
    {allocate,1,6}.
    {move,{x,4},{y,0}}.
    {move,{x,5},{x,1}}.
    {line,[{location,"merge_sort.erl",30}]}.
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
    {func_info,{atom,merge_sort},{atom,module_info},0}.
  {label,15}.
    {move,{atom,merge_sort},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 17}.
  {label,16}.
    {line,[]}.
    {func_info,{atom,merge_sort},{atom,module_info},1}.
  {label,17}.
    {move,{x,0},{x,1}}.
    {move,{atom,merge_sort},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
