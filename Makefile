# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.31

# Default target executed when no arguments are given to make.
default_target: all
.PHONY : default_target

# Allow only one "make -f Makefile2" at a time, but pass parallelism.
.NOTPARALLEL:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/saatvikl/Documents/studies/part-ii/project/main/shared/jream

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/saatvikl/Documents/studies/part-ii/project/main/shared/jream

#=============================================================================
# Targets provided globally by CMake.

# Special rule for the target edit_cache
edit_cache:
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --cyan "Running CMake cache editor..."
	/usr/bin/ccmake -S$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR)
.PHONY : edit_cache

# Special rule for the target edit_cache
edit_cache/fast: edit_cache
.PHONY : edit_cache/fast

# Special rule for the target rebuild_cache
rebuild_cache:
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --cyan "Running CMake to regenerate build system..."
	/usr/bin/cmake --regenerate-during-build -S$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR)
.PHONY : rebuild_cache

# Special rule for the target rebuild_cache
rebuild_cache/fast: rebuild_cache
.PHONY : rebuild_cache/fast

# The main all target
all: cmake_check_build_system
	$(CMAKE_COMMAND) -E cmake_progress_start /home/saatvikl/Documents/studies/part-ii/project/main/shared/jream/CMakeFiles /home/saatvikl/Documents/studies/part-ii/project/main/shared/jream//CMakeFiles/progress.marks
	$(MAKE) $(MAKESILENT) -f CMakeFiles/Makefile2 all
	$(CMAKE_COMMAND) -E cmake_progress_start /home/saatvikl/Documents/studies/part-ii/project/main/shared/jream/CMakeFiles 0
.PHONY : all

# The main clean target
clean:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/Makefile2 clean
.PHONY : clean

# The main clean target
clean/fast: clean
.PHONY : clean/fast

# Prepare targets for installation.
preinstall: all
	$(MAKE) $(MAKESILENT) -f CMakeFiles/Makefile2 preinstall
.PHONY : preinstall

# Prepare targets for installation.
preinstall/fast:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/Makefile2 preinstall
.PHONY : preinstall/fast

# clear depends
depend:
	$(CMAKE_COMMAND) -S$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR) --check-build-system CMakeFiles/Makefile.cmake 1
.PHONY : depend

#=============================================================================
# Target rules for targets named beamparser

# Build rule for target.
beamparser: cmake_check_build_system
	$(MAKE) $(MAKESILENT) -f CMakeFiles/Makefile2 beamparser
.PHONY : beamparser

# fast build rule for target.
beamparser/fast:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/build
.PHONY : beamparser/fast

beamparser.o: beamparser.cpp.o
.PHONY : beamparser.o

# target to build an object file
beamparser.cpp.o:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/beamparser.cpp.o
.PHONY : beamparser.cpp.o

beamparser.i: beamparser.cpp.i
.PHONY : beamparser.i

# target to preprocess a source file
beamparser.cpp.i:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/beamparser.cpp.i
.PHONY : beamparser.cpp.i

beamparser.s: beamparser.cpp.s
.PHONY : beamparser.s

# target to generate assembly for a file
beamparser.cpp.s:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/beamparser.cpp.s
.PHONY : beamparser.cpp.s

exceptions.o: exceptions.cpp.o
.PHONY : exceptions.o

# target to build an object file
exceptions.cpp.o:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/exceptions.cpp.o
.PHONY : exceptions.cpp.o

exceptions.i: exceptions.cpp.i
.PHONY : exceptions.i

# target to preprocess a source file
exceptions.cpp.i:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/exceptions.cpp.i
.PHONY : exceptions.cpp.i

exceptions.s: exceptions.cpp.s
.PHONY : exceptions.s

# target to generate assembly for a file
exceptions.cpp.s:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/exceptions.cpp.s
.PHONY : exceptions.cpp.s

execution.o: execution.cpp.o
.PHONY : execution.o

# target to build an object file
execution.cpp.o:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/execution.cpp.o
.PHONY : execution.cpp.o

execution.i: execution.cpp.i
.PHONY : execution.i

# target to preprocess a source file
execution.cpp.i:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/execution.cpp.i
.PHONY : execution.cpp.i

execution.s: execution.cpp.s
.PHONY : execution.s

# target to generate assembly for a file
execution.cpp.s:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/execution.cpp.s
.PHONY : execution.cpp.s

external_term.o: external_term.cpp.o
.PHONY : external_term.o

# target to build an object file
external_term.cpp.o:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/external_term.cpp.o
.PHONY : external_term.cpp.o

external_term.i: external_term.cpp.i
.PHONY : external_term.i

# target to preprocess a source file
external_term.cpp.i:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/external_term.cpp.i
.PHONY : external_term.cpp.i

external_term.s: external_term.cpp.s
.PHONY : external_term.s

# target to generate assembly for a file
external_term.cpp.s:
	$(MAKE) $(MAKESILENT) -f CMakeFiles/beamparser.dir/build.make CMakeFiles/beamparser.dir/external_term.cpp.s
.PHONY : external_term.cpp.s

# Help Target
help:
	@echo "The following are some of the valid targets for this Makefile:"
	@echo "... all (the default if no target is provided)"
	@echo "... clean"
	@echo "... depend"
	@echo "... edit_cache"
	@echo "... rebuild_cache"
	@echo "... beamparser"
	@echo "... beamparser.o"
	@echo "... beamparser.i"
	@echo "... beamparser.s"
	@echo "... exceptions.o"
	@echo "... exceptions.i"
	@echo "... exceptions.s"
	@echo "... execution.o"
	@echo "... execution.i"
	@echo "... execution.s"
	@echo "... external_term.o"
	@echo "... external_term.i"
	@echo "... external_term.s"
.PHONY : help



#=============================================================================
# Special targets to cleanup operation of make.

# Special rule to run CMake to check the build system integrity.
# No rule that depends on this can have commands that come from listfiles
# because they might be regenerated.
cmake_check_build_system:
	$(CMAKE_COMMAND) -S$(CMAKE_SOURCE_DIR) -B$(CMAKE_BINARY_DIR) --check-build-system CMakeFiles/Makefile.cmake 0
.PHONY : cmake_check_build_system

