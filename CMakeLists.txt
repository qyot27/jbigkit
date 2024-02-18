cmake_minimum_required(VERSION 3.6)

project(jbigkit VERSION 2.1)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)

add_library(jbig libjbig/jbig.c
                 libjbig/jbig.h
                 libjbig/jbig_ar.c
                 libjbig/jbig_ar.h
                 libjbig/jbig85.c
                 libjbig/jbig85.h
                 libjbig/tstcodec.c
                 libjbig/tstcodec85.c
                 libjbig/tstjoint.c )

if(NOT MSVC)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -W -Wno-unused-result")
endif()

include(GNUInstallDirs)
install(
          FILES ${CMAKE_CURRENT_SOURCE_DIR}/libjbig/jbig.h
                ${CMAKE_CURRENT_SOURCE_DIR}/libjbig/jbig_ar.h
          DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  )

install(TARGETS jbig
        LIBRARY DESTINATION lib)

# uninstall target
configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
    IMMEDIATE @ONLY)

add_custom_target(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
