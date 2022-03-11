##########################################################################
# Project Setup                                                          #
##########################################################################
cmake_minimum_required(VERSION 3.11)

if(DEFINED ENV{CMAKE_TOOLCHAIN_FILE})
  set(CMAKE_TOOLCHAIN_FILE $ENV{CMAKE_TOOLCHAIN_FILE})
endif()

project(EBus C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 99)

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_SOURCE_DIR}/cmake)
include(ASan)

IF (APPLE)
  set(CMAKE_MODULE_LINKER_FLAGS "-flat_namespace -undefined suppress")
  set(CMAKE_MACOSX_RPATH 1)
  if (NOT DEFINED ENV{SDKROOT})
    execute_process(COMMAND
      xcrun --show-sdk-path
      OUTPUT_VARIABLE APPLE_SDK_ROOT
    )
  else()
     set(APPLE_SDK_ROOT $ENV{SDKROOT})
  endif()
  set(APPLE_SDKROOT_ENV "SDKROOT=${APPLE_SDK_ROOT}")
ENDIF()

#
# Enable sanitization if environment variable is set
#
# Truth Table
#
# +----------------------+---------------+------------------+
# | SANITIZE_ERLANG_NIFS | SANITIZE_EBUS | Enable santizer? |
# +----------------------+---------------+------------------+
# | undefined            | undefined     |                0 |
# | 1                    | undefined     |                1 |
# | don't care           | 1             |                1 |
# | don't care           | 0             |                0 |
# +----------------------+---------------+------------------+
#
if(DEFINED ENV{SANITIZE_EBUS})
  if($ENV{SANITIZE_EBUS})
    set(CMAKE_BUILD_TYPE ASan CACHE STRING "Choose the type of build." FORCE)
  else()
    unset(CMAKE_BUILD_TYPE CACHE)
  endif()
elseif(DEFINED ENV{SANITIZE_ERLANG_NIFS})
  if($ENV{SANITIZE_ERLANG_NIFS})
    set(CMAKE_BUILD_TYPE ASan CACHE STRING "Choose the type of build." FORCE)
  else()
    unset(CMAKE_BUILD_TYPE CACHE)
  endif($ENV{SANITIZE_ERLANG_NIFS})
endif()

#
# Set a default build type if none was specified
#
set(default_build_type "Release")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Choose the type of build." FORCE)
endif()


##########################################################################
# Static Analysis                                                        #
##########################################################################
if(CMAKE_VERSION VERSION_GREATER 3.9)
  find_program(CPPCHECK
    NAMES cppcheck
    DOC   "Path to cppcheck executable"
    )
  if(CPPCHECK)
    message(STATUS "Found cppcheck: ${CPPCHECK}")
    set(CMAKE_C_CPPCHECK ${CPPCHECK}
      --enable=all
      --inline-suppr
      --inconclusive
      --quiet
      --std=c${CMAKE_C_STANDARD}
      --suppress=missingInclude
      --suppress=unusedFunction
      --template=gcc
      )
  endif(CPPCHECK)
endif()

if(CMAKE_VERSION VERSION_GREATER 3.5)
  find_program(CLANG_TIDY
    NAMES clang-tidy
    DOC   "Path to clang-tidy executable"
    )
  if(CLANG_TIDY)
    message(STATUS "Found clang-tidy: ${CLANG_TIDY}")
    set(CMAKE_C_CLANG_TIDY "${CLANG_TIDY}")
  endif(CLANG_TIDY)
endif()


##########################################################################
# NIF                                                                    #
##########################################################################
find_package(Erlang REQUIRED)
find_package(DBus REQUIRED)
add_library(ebus MODULE
  ebus_connection.c
  ebus_error.c
  ebus_filter.c
  ebus_message.c
  ebus_message_append_arg.c
  ebus_message_list_args.c
  ebus_nif.c
  ebus_object.c
  ebus_timeout.c
  ebus_watch.c
)
target_link_libraries(ebus
  PRIVATE
  Erlang::Erlang
  DBus::DBus
  )

install(TARGETS ebus DESTINATION ${CMAKE_SOURCE_DIR}/../priv)
