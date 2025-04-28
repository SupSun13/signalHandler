#include "../include/digitalSignal.h"


float DigitalSignal::operator[](float time) const {
    float currentTime = 0;
    bool isLow = true;

    for (float duration : signal) {
        if (time < currentTime + duration) {
            return isLow ? lowSignalLevel : highSignalLevel;
        }
        currentTime += duration;
        isLow = !isLow;
    }
    return isLow ? lowSignalLevel : highSignalLevel;
}

void DigitalSignal::setLowSignalLevel(float lowSignalLevel) {
    this->lowSignalLevel = lowSignalLevel;
}

void DigitalSignal::setHighSignalLevel(float highSignalLevel) {
    this->highSignalLevel = highSignalLevel;
}


Signal* DigitalSignal::operator+(const Signal& other) const {
    const DigitalSignal* digitalSignal = dynamic_cast<const DigitalSignal*>(&other);
    if (!digitalSignal) {
        throw std::logic_error("Operation not supported for non-digital signals.");
    }

    std::vector<float> concatenatedSignal = signal;

    if (!signal.empty() && !digitalSignal->signal.empty()) {
        bool lastLevelFirstSignal = (signal.size() % 2 == 1);
        bool firstLevelSecondSignal = true;
        if (lastLevelFirstSignal == firstLevelSecondSignal) {
            concatenatedSignal.back() += digitalSignal->signal.front();
            concatenatedSignal.insert(concatenatedSignal.end(), digitalSignal->signal.begin() + 1, digitalSignal->signal.end());
        } else {
            concatenatedSignal.insert(concatenatedSignal.end(), digitalSignal->signal.begin(), digitalSignal->signal.end());
        }
    }

    return new DigitalSignal("concatenated", concatenatedSignal); // Новый сигнал с уникальным ID
}



// DigitalSignal DigitalSignal::operator+(const DigitalSignal& other) const {
//     std::vector<float> concatenatedSignal = signal;

//     if (!signal.empty() && !other.signal.empty()) {
//         bool lastLevelFirstSignal = (signal.size() % 2 == 1);
//         bool firstLevelSecondSignal = true;
        
//         if (lastLevelFirstSignal == firstLevelSecondSignal) {
//             concatenatedSignal.back() += other.signal.front();
//             concatenatedSignal.insert(concatenatedSignal.end(), other.signal.begin() + 1, other.signal.end());
//         } else {
//             concatenatedSignal.insert(concatenatedSignal.end(), other.signal.begin(), other.signal.end());
//         }
//     }

//     return DigitalSignal(concatenatedSignal);
// }

float DigitalSignal::getValueAt(float time) const {
    return operator[](time);
}

std::shared_ptr<Signal> DigitalSignal::concatenate(const Signal& other) const {
    const DigitalSignal* otherDigital = dynamic_cast<const DigitalSignal*>(&other);
    if (!otherDigital) {
        throw std::logic_error("Concatenation supports only DigitalSignal.");
    }
    return std::make_shared<DigitalSignal>((*this) + (*otherDigital));
}

std::ostream& operator<<(std::ostream& os, const DigitalSignal& ds) {
    os << static_cast<const Signal&>(ds); // Вывод базового класса
    os << ", Signal Levels: ";
    for (const auto& level : ds.signal) {
        os << level << " ";
    }
    return os;
}
