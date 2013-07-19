# @author Can Erdogan
# @date Jan 15, 2012

cmake_minimum_required(VERSION 2.6)
project(somatic)

# Set the C99 standard for the C files
set(CMAKE_INSTALL_PREFIX /usr)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -fpermissive")

# Guard against in-source builds
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "In-source builds are not allowed. You may need to remove CMakeCache.txt.")
endif()

# Link to somatic, amino and ach
link_libraries(amino protobuf-c ach ntcan stdc++)

# Include the project files and the common helpers for all the experiments
include_directories(include)
include_directories(include/somatic)

# Collect the source, script and fortran files
file(GLOB main_source "src/*.cpp" "src/*.c")
file(GLOB scripts_source "exe/*.cpp" " exe/*.c")
LIST(SORT scripts_source)

# Build the library and set the executables that use it (have to hardcode for now)
add_library(somatic SHARED ${main_source})

# Build scripts
message(STATUS "\n-- SCRIPTS: ")
foreach(script_src_file ${scripts_source})
	get_filename_component(script_base ${script_src_file} NAME_WE)
	message(STATUS "Adding script ${script_src_file} with base name ${script_base}" )
	add_executable(${script_base} ${script_src_file})
	target_link_libraries(${script_base} somatic) 
	add_custom_target(${script_base}.run ${script_base} ${ARGN})
	install(TARGETS ${script_base} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
endforeach(script_src_file)
message(STATUS " ")

# Install the files
FILE(GLOB files "include/somatic/*.h")
install(TARGETS somatic DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
install(FILES include/somatic.pb-c.h DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
install(FILES include/somatic.h DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
install(FILES ${files} DESTINATION 	${CMAKE_INSTALL_PREFIX}/include/somatic)
install(PROGRAMS etc/init.d/somatic DESTINATION /etc/init.d)

# Setup a debian package with cpack
set(CPACK_PACKAGE_NAME "somatic")
set(CPACK_SYSTEM_NAME "i386")
if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    set(CPACK_SYSTEM_NAME "amd64")    
endif()
set(CPACK_PACKAGE_VERSION_MAJOR 0)
set(CPACK_PACKAGE_VERSION_MINOR 0)
set(CPACK_PACKAGE_VERSION_PATCH 0)
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libach-dev, libamino-dev, libprotobuf-c0")

set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}_${CPACK_SYSTEM_NAME}")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Saul Reynolds-Haertle <saulrh@gatech.edu>")

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Somatic is a communication library that wraps ach with protobuf-c structure and message types for convenience")
set(CPACK_PACKAGE_VENDOR "Georgia Tech Humanoids Lab")

include(CPack)
