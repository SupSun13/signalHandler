#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/functional.h>
#include "signal.h"
#include "./include/sinusoidalSignal.h"
#include "./include/digitalSignal.h"
#include "./include/analogSignal.h"
#include "./include/signalStorage.h"
#include "./include/signalOperation.h"
#include "./include/processingPipeline.h"

namespace py = pybind11;

PYBIND11_MODULE(signal_lib, m) {
    // Базовый класс Signal
    py::class_<Signal, std::shared_ptr<Signal>>(m, "Signal")
        .def("getValueAt", &Signal::getValueAt)
        .def("getId", &Signal::getId)
        .def("__add__", &Signal::operator+)
        .def("__getitem__", &Signal::operator[])
        .def("concatenate", &Signal::concatenate)
        .def("clone", &Signal::clone);

    // SinusoidalSignal
    py::class_<SinusoidalSignal, Signal, std::shared_ptr<SinusoidalSignal>>(m, "SinusoidalSignal")
        .def(py::init<const std::string&, float, float, float>())
        .def("getValueAt", &SinusoidalSignal::getValueAt);

    // DigitalSignal
    py::class_<DigitalSignal, Signal, std::shared_ptr<DigitalSignal>>(m, "DigitalSignal")
        .def(py::init<const std::string&, const std::vector<float>&>())
        .def("setLowSignalLevel", &DigitalSignal::setLowSignalLevel)
        .def("setHighSignalLevel", &DigitalSignal::setHighSignalLevel);

    // AnalogSignal
    py::class_<AnalogSignal, Signal, std::shared_ptr<AnalogSignal>>(m, "AnalogSignal")
        .def(py::init<const std::string&, const std::vector<float>&, const std::vector<float>&>());

    // SignalStorage
    py::class_<SignalStorage>(m, "SignalStorage")
        .def(py::init<>())
        .def("addSignal", &SignalStorage::addSignal)
        .def("findSignalById", &SignalStorage::findSignalById)
        .def("findLongestSignal", &SignalStorage::findLongestSignal)
        .def("findMinMaxValues", &SignalStorage::findMinMaxValues);

    // SignalOperation
    py::class_<SignalOperation>(m, "SignalOperation")
        .def(py::init<const std::string&, const std::function<std::shared_ptr<Signal>(const Signal&, const Signal&)>&>())
        .def("execute", &SignalOperation::execute)
        .def("getName", &SignalOperation::getName);

    // ProcessingPipeline
    py::class_<ProcessingPipeline>(m, "ProcessingPipeline")
        .def(py::init<SignalStorage&>())
        .def("addOperation", &ProcessingPipeline::addOperation)
        .def("validate", &ProcessingPipeline::validate)
        .def("execute", &ProcessingPipeline::execute);
}


//QGraphicsView QGraphicsScene
//Canvas TKinter