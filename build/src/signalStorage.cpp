#include "../include/signalStorage.h"
#include <limits>

std::shared_ptr<Signal> SignalStorage::findLongestSignal() const {
    if (signals.empty()) return nullptr;

    auto longestSignal = signals.front();
    for (const auto& signal : signals) {
        if (signal->getValueAt(0) > longestSignal->getValueAt(0)) {
            longestSignal = signal;
        }
    }
    return longestSignal;
}

std::pair<float, float> SignalStorage::findMinMaxValues() const {
    if (signals.empty()) return {0.0f, 0.0f};

    float minValue = std::numeric_limits<float>::max();
    float maxValue = std::numeric_limits<float>::lowest();

    for (const auto& signal : signals) {
        for (float time = 0.0f; time <= 10.0f; time += 1.0f) { // ВРЕМЕННО значения от 0 до 10 с шагом 1
            float value = signal->getValueAt(time);
            minValue = std::min(minValue, value);
            maxValue = std::max(maxValue, value);
        }
    }
    return {minValue, maxValue};
}

std::shared_ptr<Signal> SignalStorage::findSignalById(const std::string& id) const {
    auto it = std::find_if(signals.begin(), signals.end(),
                           [&id](const std::shared_ptr<Signal>& signal) {
                               return signal->getId() == id;
                           });
    return (it != signals.end()) ? *it : nullptr;
}

SignalStorage::SignalStorage(const SignalStorage& other) {
    signals.reserve(other.signals.size());
    for (const auto& signal : other.signals) {
        signals.push_back(std::shared_ptr<Signal>(signal->clone()));
    }
}

SignalStorage& SignalStorage::operator=(const SignalStorage& other) {
    if (this != &other) {
        signals.clear();
        signals.reserve(other.signals.size());
        for (const auto& signal : other.signals) {
            signals.push_back(std::shared_ptr<Signal>(signal->clone()));
        }
    }
    return *this;
}