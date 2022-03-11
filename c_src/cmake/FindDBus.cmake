# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindDBus
-------

Finds the DBus library.

Imported Targets
^^^^^^^^^^^^^^^^

``DBus::DBus``
  DBus imported target library.

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``DBus_FOUND``
  True if the system has the DBus library.

``DBus_INCLUDE_DIR``
  Path to DBus headers.

``DBus_ARCH_INCLUDE_DIR``
  Architecture-specific headers.

#]=======================================================================]
find_package(PkgConfig)
pkg_check_modules(PC_DBus QUIET dbus-1)

find_path(DBus_INCLUDE_DIR
  NAMES dbus/dbus.h
  PATH_SUFFIXES dbus-1.0
  PATHS ${PC_DBus_INCLUDE_DIR}
  )

find_path(DBus_ARCH_INCLUDE_DIR
  NAMES dbus/dbus-arch-deps.h
  PATH_SUFFIXES dbus-1.0/include
  PATHS ${PC_DBus_LIBDIR} /usr/lib/${CMAKE_LIBRARY_ARCHITECTURE}
  )

find_library(DBus_LIBRARY
  NAMES dbus-1
  PATHS ${PC_DBus_LIBDIR}
  )

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(DBus
  FOUND_VAR DBus_FOUND
  REQUIRED_VARS DBus_LIBRARY DBus_INCLUDE_DIR DBus_ARCH_INCLUDE_DIR
  )

if(DBus_FOUND AND NOT TARGET DBus::DBus)
  add_library(DBus::DBus UNKNOWN IMPORTED)
  set_target_properties(DBus::DBus PROPERTIES
    IMPORTED_LOCATION ${DBus_LIBRARY}
    INTERFACE_INCLUDE_DIRECTORIES "${DBus_INCLUDE_DIR};${DBus_ARCH_INCLUDE_DIR}"
    )
endif()
