#ifndef ANALOG_SIGNAL_H
#define ANALOG_SIGNAL_H

#include "signal.h"

class AnalogSignal : public Signal {
private:
    std::vector<float> values;
    std::vector<float> times;

public:
    AnalogSignal(const std::string& id, const std::vector<float>& values, const std::vector<float>& times)
        : Signal(id), values(values), times(times) {}
    AnalogSignal(Signal* baseSignal)
    : Signal(baseSignal->getId()) {
    AnalogSignal* analogSignal = dynamic_cast<AnalogSignal*>(baseSignal);
    if (analogSignal) {
        values = analogSignal->values;
        times = analogSignal->times;
    } else {
        values.clear();
        times.clear();
    }
    }
    ~AnalogSignal() override = default;
    

    AnalogSignal* clone() const override {
        return new AnalogSignal(*this);
    }

    float operator[](float time) const override;
    Signal* operator+(const Signal& other) const override;
    float getValueAt(float time) const override; 
    std::shared_ptr<Signal> concatenate(const Signal& other) const override; 
};

#endif
