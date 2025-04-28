cmake_minimum_required(VERSION 3.12)
project(signal_lib)

set(pybind11_DIR "/Users/sergej/PyCharmMiscProject/.venv/lib/python3.13/site-packages/pybind11/share/cmake/pybind11")
find_package(pybind11 REQUIRED)
find_package(Python3 REQUIRED COMPONENTS Development)

set(SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/src/digitalSignal.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/analogSignal.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/signalStorage.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/processingPipeline.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/bindings.cpp
)

pybind11_add_module(signal_lib ${SOURCES})

target_include_directories(signal_lib PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${Python3_INCLUDE_DIRS}
    ${pybind11_INCLUDE_DIRS}
)

target_link_libraries(signal_lib PRIVATE
    pybind11::module
    Python3::Python
)