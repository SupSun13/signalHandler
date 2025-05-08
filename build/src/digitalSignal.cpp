// analogSignal.cpp
#include "../include/analogSignal.h"

// --------------------
// Node
// --------------------
AnalogSignal::Node::Node(float t, float v)
  : time(t), value(v), left(nullptr), right(nullptr)
{}

// --------------------
// Построение дерева
// --------------------
AnalogSignal::Node* AnalogSignal::buildTree(int l, int r) {
    if (l > r) return nullptr;
    int m = (l + r) / 2;
    Node* node = new Node(times[m], values[m]);
    node->left  = buildTree(l, m - 1);
    node->right = buildTree(m + 1, r);
    return node;
}

// --------------------
// Floor‑поиск
// --------------------
float AnalogSignal::searchFloor(Node* node, float t, float seenValue) const {
    if (!node) return seenValue;
    if (t == node->time) {
        return node->value;
    } else if (t < node->time) {
        return searchFloor(node->left, t, seenValue);
    } else {
        // t > node->time — обновляем seenValue
        return searchFloor(node->right, t, node->value);
    }
}

// --------------------
// Удаление дерева
// --------------------
void AnalogSignal::deleteTree(Node* node) {
    if (!node) return;
    deleteTree(node->left);
    deleteTree(node->right);
    delete node;
}

// --------------------
// Конструкторы / деструктор / clone
// --------------------
AnalogSignal::AnalogSignal(const std::string& id,
                           const std::vector<float>& vals,
                           const std::vector<float>& ts)
  : Signal(id), values(vals), times(ts)
{
    // Предполагаем: times уже отсортирован и совпадает по размеру с values
    root = buildTree(0, int(times.size()) - 1);
}

AnalogSignal::AnalogSignal(const AnalogSignal& other)
  : Signal(other.getId())
  , values(other.values)
  , times(other.times)
{
    root = buildTree(0, int(times.size()) - 1);
}

AnalogSignal::~AnalogSignal() {
    deleteTree(root);
}

AnalogSignal* AnalogSignal::clone() const {
    return new AnalogSignal(*this);
}

// --------------------
// Доступ по времени
// --------------------
float AnalogSignal::operator[](float time) const {
    if (times.empty()) {
        throw std::out_of_range("Empty signal");
    }
    // граничные случаи
    if (time <= times.front())    return values.front();
    if (time >= times.back())     return values.back();
    // floor‑поиск в BST
    return searchFloor(root, time, values.front());
}

float AnalogSignal::getValueAt(float time) const {
    return (*this)[time];
}

// --------------------
// Сложение и конкатенация — без изменений
// --------------------
Signal* AnalogSignal::operator+(const Signal& other) const {
    const AnalogSignal* o = dynamic_cast<const AnalogSignal*>(&other);
    if (!o) throw std::logic_error("Can only add AnalogSignal");
    std::vector<float> newValues(values.size());
    for (size_t i = 0; i < newValues.size(); ++i)
        newValues[i] = values[i] + o->values[i];
    return new AnalogSignal("Sum", newValues, times);
}

std::shared_ptr<Signal> AnalogSignal::concatenate(const Signal& other) const {
    const AnalogSignal* o = dynamic_cast<const AnalogSignal*>(&other);
    if (!o) throw std::logic_error("Can only concatenate AnalogSignal");
    std::vector<float> newValues = values;
    newValues.insert(newValues.end(), o->values.begin(), o->values.end());
    std::vector<float> newTimes = times;
    float offset = newTimes.back();
    for (float t : o->times) newTimes.push_back(t + offset);
    return std::make_shared<AnalogSignal>("Concat", newValues, newTimes);
}


std::vector<float> AnalogSignal::getValues() const {
    return values;
}

std::vector<float> AnalogSignal::getTimes() const {
    return times;
}
