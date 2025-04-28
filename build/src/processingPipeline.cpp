#include "../include/processingPipeline.h"
#include <stdexcept>



void ProcessingPipeline::addOperation(const std::string& opName, const std::string& input1, const std::string& input2, const std::string& resultId) {
    operations.emplace_back(opName, input1, input2, resultId);
}

void ProcessingPipeline::validate() const {
    for (const auto& [opName, input1, input2, resultId] : operations) {
        if (!storage.findSignalById(input1) || !storage.findSignalById(input2)) {
            throw std::logic_error("Invalid operation: Signal not found.");
        }
    }
}

void ProcessingPipeline::execute() {
    for (const auto& [opName, input1, input2, resultId] : operations) {
        auto signal1 = storage.findSignalById(input1);
        auto signal2 = storage.findSignalById(input2);
        auto result = SignalOperation(opName, [](const Signal& s1, const Signal& s2) {
            return s1.concatenate(s2);
        }).execute(*signal1, *signal2);

        temporarySignals[resultId] = result;
    }
}
