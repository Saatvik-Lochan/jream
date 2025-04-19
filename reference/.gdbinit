set substitute-path /home/saatvikl/Documents/studies/part-ii/project/main/shared/ /home/ubuntu/shared

define setbs
    if $argc < 2
        printf "Usage: setup_bps <bp1> <bp2>\n"
    else
        break $arg0
        break $arg1
        set $second = $bpnum
        set $first = $second - 1

        disable $second
        commands $first
            enable $second
            continue
        end

	commands $second
	    disable $second
        end
    end
end

define asm
  b setup_and_go_label
end

define easm
    if $argc < 1
        printf "Usage: easm <line num>\n"
    else
	setbs test_main.cpp:$arg0 setup_and_go_label
    end
end

define ets
    if $argc < 1
        printf "Usage: ets <line num>\n"
    else
	setbs test_main.cpp:$arg0 translate_code_section
    end
end

define set2
    setbs test_main.cpp:$arg0 $arg1
end

define sat
	b print_op_name if (op_code == $arg0)
end

define pe 
  p io_write($arg0)
end

define px
  p io_write((*(uint64_t *)$s0) + $arg0)
end
