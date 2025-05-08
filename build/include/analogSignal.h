// analogSignal.h
#ifndef ANALOG_SIGNAL_H
#define ANALOG_SIGNAL_H

#include "signal.h"
#include <vector>
#include <string>
#include <memory>
#include <stdexcept>

// ----------------------------------------------------------------
// AnalogSignal: хранит два параллельных вектора times[] и values[]
// Построение сбалансированного BST из середины отсортированного times/
// и поиск “floor”‑значения (последнее <= запрошенного).
// ----------------------------------------------------------------
class AnalogSignal : public Signal {
private:
    std::vector<float> values;
    std::vector<float> times;

    // Узел нашего BST
    struct Node {
        float time;
        float value;
        Node* left;
        Node* right;
        Node(float t, float v);
    };
    Node* root;

    // Построить сбалансированное дерево из диапазона [l..r] в массиве
    Node* buildTree(int l, int r);

    // Рекурсивный floor‑поиск: при движении вправо сохраняем последнее ≤ t
    float searchFloor(Node* node, float t, float seenValue) const;

    // Рекурсивно удалить все узлы (в деструкторе)
    void deleteTree(Node* node);

public:
    // Конструкторы / деструктор / clone
    AnalogSignal(const std::string& id,
                 const std::vector<float>& values,
                 const std::vector<float>& times);
    AnalogSignal(const AnalogSignal& other);
    ~AnalogSignal() override;
    AnalogSignal* clone() const override;

    // Основные операции
    std::vector<float> getValues() const;
    std::vector<float> getTimes()  const;
    float operator[](float time) const override;
    float getValueAt(float time) const override;

    // Операции над сигналами
    Signal* operator+(const Signal& other) const override;
    std::shared_ptr<Signal> concatenate(const Signal& other) const override;
};

#endif // ANALOG_SIGNAL_H
