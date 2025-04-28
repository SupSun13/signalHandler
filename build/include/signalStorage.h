#ifndef SIGNAL_STORAGE_H
#define SIGNAL_STORAGE_H

#include "signal.h"
#include <vector>
#include <memory>
#include <algorithm>

class SignalStorage {
private:
    std::vector<std::shared_ptr<Signal>> signals;

public:
    SignalStorage() = default;
    SignalStorage(const SignalStorage& other);
    SignalStorage& operator=(const SignalStorage& other);
    void addSignal(const std::shared_ptr<Signal>& signal) {
        signals.push_back(signal);
    }

    std::shared_ptr<Signal> findSignalById(const std::string& id) const;
    std::shared_ptr<Signal> findLongestSignal() const;
    std::pair<float, float> findMinMaxValues() const;
    
};

#endif
