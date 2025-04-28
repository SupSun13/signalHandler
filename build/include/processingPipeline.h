#ifndef PROCESSING_PIPELINE_H
#define PROCESSING_PIPELINE_H

#include "signalStorage.h"
#include "signalOperation.h"
#include <vector>
#include <map>
#include <string>

class ProcessingPipeline {
private:
    SignalStorage& storage;
    std::vector<std::tuple<std::string, std::string, std::string, std::string>> operations;
    std::map<std::string, std::shared_ptr<Signal>> temporarySignals;

public:
    ProcessingPipeline(SignalStorage& storage) : storage(storage) {}

 
    void addOperation(const std::string& opName, const std::string& input1, const std::string& input2, const std::string& resultId);
    void validate() const;
    void execute();

    const std::vector<std::tuple<std::string, std::string, std::string, std::string>>& getOperations() const { return operations; }
};

#endif
