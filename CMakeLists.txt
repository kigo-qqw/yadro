cmake_minimum_required(VERSION 3.20)

project(yadro LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_compile_options(-Weverything -Werror -Wfatal-errors)
add_compile_options(
  -Wno-c++98-compat
  -Wno-c++98-compat-pedantic
  -Wno-pre-c++20-compat-pedantic
  -Wno-covered-switch-default
  -Wno-padded
  -Wno-unsafe-buffer-usage

  -Wno-missing-prototypes
  -Wno-unused-parameter
  -Wno-weak-vtables

  -fsanitize=undefined
  -fsanitize=address
)

add_link_options(
  -fsanitize=undefined
  -fsanitize=address
)

add_executable(${PROJECT_NAME})
target_sources(${PROJECT_NAME}
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp
)
