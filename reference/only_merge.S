{module, only_merge}.  %% version = 0

{exports, [{merge,2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 10}.


{function, merge, 2, 2}.
  {label,1}.
    {line,[{location,"only_merge.erl",5}]}.
    {func_info,{atom,only_merge},{atom,merge},2}.
  {label,2}.
    {test,is_nil,{f,3},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.
  {label,3}.
    {test,is_nil,{f,4},[{x,1}]}.
    return.
  {label,4}.
    {test,is_nonempty_list,{f,1},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_nonempty_list,{f,1},[{x,1}]}.
    {get_list,{x,1},{x,4},{x,5}}.
    {test,is_ge,{f,5},[{x,4},{x,2}]}.
    {allocate,1,4}.
    {move,{x,2},{y,0}}.
    {move,{x,3},{x,0}}.
    {line,[{location,"only_merge.erl",9}]}.
    {call,2,{f,2}}. % merge/2
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,5}.
    {allocate,1,6}.
    {move,{x,4},{y,0}}.
    {move,{x,5},{x,1}}.
    {line,[{location,"only_merge.erl",10}]}.
    {call,2,{f,2}}. % merge/2
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


{function, module_info, 0, 7}.
  {label,6}.
    {line,[]}.
    {func_info,{atom,only_merge},{atom,module_info},0}.
  {label,7}.
    {move,{atom,only_merge},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 9}.
  {label,8}.
    {line,[]}.
    {func_info,{atom,only_merge},{atom,module_info},1}.
  {label,9}.
    {move,{x,0},{x,1}}.
    {move,{atom,only_merge},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
