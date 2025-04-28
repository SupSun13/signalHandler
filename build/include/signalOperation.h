#ifndef SIGNAL_OPERATION_H
#define SIGNAL_OPERATION_H

#include "signal.h"
#include <memory>
#include <functional>
#include <string>

class SignalOperation {
private:
    std::string operationName;
    std::function<std::shared_ptr<Signal>(const Signal&, const Signal&)> operation;

public:
    SignalOperation(const std::string& name, const std::function<std::shared_ptr<Signal>(const Signal&, const Signal&)>& op)
        : operationName(name), operation(op) {}

    std::shared_ptr<Signal> execute(const Signal& signal1, const Signal& signal2) const {
        return operation(signal1, signal2);
    }

    const std::string& getName() const { return operationName; }
};

#endif
