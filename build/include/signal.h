#ifndef SIGNAL_H
#define SIGNAL_H

#include <vector>
#include <string>
#include <memory>
#include <stdexcept>
#include <iostream>

class Signal {
protected:
    std::string id;

public:
    Signal(const std::string& id) : id(id) {}
    virtual ~Signal() = default;

    virtual float operator[](float time) const = 0;
    virtual Signal* operator+(const Signal& other) const = 0;

    const std::string& getId() const { return id; }
    virtual float getValueAt(float time) const = 0;
    virtual std::shared_ptr<Signal> concatenate(const Signal& other) const = 0;
    virtual Signal* clone() const = 0;
    
    friend std::ostream& operator<<(std::ostream& os, const Signal& signal) {
        os << "Signal ID: " << signal.id;
        return os;
    }
};

#endif
