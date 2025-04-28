// #ifndef DIGITAL_SIGNAL_H
// #define DIGITAL_SIGNAL_H

// #include "signal.h" // Подключаем базовый класс
// #include <vector>


// class DigitalSignal : public Signal {
// private:
//     float lowSignalLevel = 0;
//     float highSignalLevel = 1;
//     std::vector<float> signal; // Массив длительностей уровней

// public:
//     // Конструкторы
//     DigitalSignal(const std::string& id, const std::vector<float>& signal)
//         : Signal(id), signal(signal) {} 
//     ~DigitalSignal() noexcept override = default; // Деструктор с noexcept

//     // Установить уровни сигнала
//     void setLowSignalLevel(float lowSignalLevel);
//     void setHighSignalLevel(float highSignalLevel);

//     // Оператор []
//     float operator[](float time) const;

//     // Оператор конкатенации
//     DigitalSignal operator+(const DigitalSignal& other) const;

//     // Переопределение виртуальных методов базового класса Signal
//     float getValueAt(float time) const override;
//     std::shared_ptr<Signal> concatenate(const Signal& other) const override;
// };

// #endif

#ifndef DIGITAL_SIGNAL_H
#define DIGITAL_SIGNAL_H

#include "signal.h"
#include <vector>
class DigitalSignal : public Signal {
private:

    float lowSignalLevel = 0;
    float highSignalLevel = 1;
    std::vector<float> signal;

public:
    DigitalSignal(const std::string& id, const std::vector<float>& signal)
        : Signal(id), signal(signal) {} 
    DigitalSignal(Signal* baseSignal) 
    : Signal(baseSignal->getId()), signal({0.0}) {
        DigitalSignal* ds = dynamic_cast<DigitalSignal*>(baseSignal);
        if (ds) {
            signal = ds->signal;
        } else {
            throw std::invalid_argument("Cannot create DigitalSignal from non-DigitalSignal object");
        }}
    ~DigitalSignal() override = default;

    void setLowSignalLevel(float lowSignalLevel);
    void setHighSignalLevel(float highSignalLevel);

    DigitalSignal* clone() const override {
        return new DigitalSignal(*this);
    }
    
    float operator[](float time) const override;
    Signal* operator+(const Signal& other) const override;
    float getValueAt(float time) const override; 
    std::shared_ptr<Signal> concatenate(const Signal& other) const override;
    friend std::ostream& operator<<(std::ostream& os, const DigitalSignal& ds);
};

#endif
