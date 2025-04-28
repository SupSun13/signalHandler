#include "../include/analogSignal.h"


float AnalogSignal::operator[](float time) const {
    for (size_t i = 0; i < values.size() - 1; ++i) {
        if (time >= times[i] && time < times[i + 1]) {
            return values[i];
        }
    }
    throw std::out_of_range("Time not found in AnalogSignal");
}

Signal* AnalogSignal::operator+(const Signal& other) const {
    const AnalogSignal* otherAnalog = dynamic_cast<const AnalogSignal*>(&other);
    if (typeid(other) != typeid(*this)) {
        throw std::logic_error("You can only add AnalogSignals");
    }

        std::vector<float> newValues(values.size());
    for (size_t i = 0; i < newValues.size(); ++i) {
        newValues[i] = values[i] + otherAnalog->values[i];
    }
    return new AnalogSignal("Result", newValues, times);
}

float AnalogSignal::getValueAt(float time) const {
    if (time <= times.front()) return values.front();
    if (time >= times.back()) return values.back();

    for (size_t i = 0; i < times.size() - 1; ++i) {
        if (time >= times[i] && time < times[i + 1]) {
            float t1 = times[i], t2 = times[i + 1];
            float v1 = values[i], v2 = values[i + 1];
            return v1 + (time - t1) * (v2 - v1) / (t2 - t1);
        }
    }
    return values.back();
}


std::shared_ptr<Signal> AnalogSignal::concatenate(const Signal& other) const {
    const AnalogSignal* otherAnalog = dynamic_cast<const AnalogSignal*>(&other);
    if (!otherAnalog) {
        throw std::logic_error("Concatenation supports only AnalogSignal.");
    }

    std::vector<float> newValues = values;
    newValues.insert(newValues.end(), otherAnalog->values.begin(), otherAnalog->values.end());

    std::vector<float> newTimes = times;
    float offset = newTimes.back();
    for (float t : otherAnalog->times) {
        newTimes.push_back(t + offset);
    }

    return std::make_shared<AnalogSignal>("Concatenated", newValues, newTimes);
}