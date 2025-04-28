#ifndef SINUSOIDAL_SIGNAL_H
#define SINUSOIDAL_SIGNAL_H

#include "signal.h"
#include <cmath>
#include <stdexcept>

class SinusoidalSignal : public Signal {
private:
    float amplitude;
    float period;
    float phase;

public:
    SinusoidalSignal(const std::string& id, float amplitude, float period, float phase)
        : Signal(id), amplitude(amplitude), period(period), phase(phase) {}
    ~SinusoidalSignal() override = default;

    SinusoidalSignal* clone() const override {
        return new SinusoidalSignal(*this);
    }


    float getValueAt(float time) const override {
        return amplitude * std::sin((2 * M_PI * time / period) + phase);
    }

    float operator[](float time) const override {
        return amplitude * std::sin((2 * M_PI * time / period) + phase);
    }

    Signal* operator+(const Signal& other) const override {
        throw std::logic_error("Concatenation not supported for sinusoidal signals.");
    }

    std::shared_ptr<Signal> concatenate(const Signal& other) const override {
        throw std::logic_error("Concatenation not supported for sinusoidal signals.");
    }

    friend std::ostream& operator<<(std::ostream& os, const SinusoidalSignal& ss) {
        os << static_cast<const Signal&>(ss);
        os << ", Amplitude: " << ss.amplitude
           << ", Period: " << ss.period
           << ", Phase: " << ss.phase;
        return os;
    }
};

#endif
