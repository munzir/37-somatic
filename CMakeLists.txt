# @author Can Erdogan
# @date Jan 15, 2012

cmake_minimum_required(VERSION 2.6)
project(forwardKinematics)

# Set the C99 standard for the C files
set(CMAKE_INSTALL_PREFIX /usr)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -fpermissive")

# Guard against in-source builds
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "In-source builds are not allowed. You may need to remove CMakeCache.txt.")
endif()

# Link to somatic, amino and ach
link_libraries(amino protobuf-c ach ntcan pcio stdc++)

# Include the project files and the common helpers for all the experiments
include_directories(src)

# Collect the source, script and fortran files
file(GLOB main_source "src/*.cpp" "src/*.c")
file(GLOB scripts_source "exe/*.cpp" " exe/*.c")
LIST(SORT scripts_source)

# Build the library and set the executables that use it (have to hardcode for now)
add_library(Main SHARED ${main_source})

# Build scripts
message(STATUS "\n-- SCRIPTS: ")
foreach(script_src_file ${scripts_source})
	get_filename_component(script_base ${script_src_file} NAME_WE)
	message(STATUS "Adding script ${script_src_file} with base name ${script_base}" )
	add_executable(${script_base} ${script_src_file})
	target_link_libraries(${script_base} Main) 
	add_custom_target(${script_base}.run ${script_base} ${ARGN})
endforeach(script_src_file)
message(STATUS " ")
