#include <iostream>
#include <vector>
#include <memory>
#include "digitalSignal.h"
#include "analogSignal.h"
#include "sinusoidalSignal.h"
#include "signalStorage.h"
#include "signalOperation.h"
#include "processingPipeline.h"

void testDigitalSignal() {
    std::cout << "Testing DigitalSignal..." << std::endl;

    std::vector<float> signal1 = {1, 2, 3};
    DigitalSignal ds1("ds1", signal1);

    // Проверка оператора []
    std::cout << "Value at time 0: " << ds1[0] << std::endl;
    std::cout << "Value at time 2: " << ds1[2] << std::endl;

    // Проверка concatenation
    DigitalSignal ds2("ds2", {4, 5});
    DigitalSignal concatenated = ds1 + ds2;
    std::cout << "Concatenated signal value at time 4: " << concatenated[4] << std::endl;

    // Проверка getValueAt
    std::cout << "Value at time 1.5: " << ds1.getValueAt(1.5) << std::endl;
}

void testAnalogSignal() {
    std::cout << "Testing AnalogSignal..." << std::endl;

    std::vector<float> values = {1.0, 2.0, 3.0};
    std::vector<float> times = {0.0, 1.0, 2.0};
    AnalogSignal as("as", values, times);

    // Проверка оператора []
    std::cout << "Value at time 1.0: " << as[1.0] << std::endl;

    // Проверка operator+
    try {
        AnalogSignal result = as + as;
        std::cout << "Concatenated AnalogSignal created successfully!" << std::endl;
    } catch (const std::exception& e) {
        std::cout << "AnalogSignal concatenation failed: " << e.what() << std::endl;
    }
}

void testSinusoidalSignal() {
    std::cout << "Testing SinusoidalSignal..." << std::endl;

    SinusoidalSignal ss("ss", 1.0, 2.0, 0.0);

    // Проверка оператора []
    std::cout << "Value at time 0.5: " << ss[0.5] << std::endl;

    // Проверка operator+
    try {
        Signal* result = ss + ss;
    } catch (const std::exception& e) {
        std::cout << "SinusoidalSignal concatenation not supported: " << e.what() << std::endl;
    }
}

void testSignalStorage() {
    std::cout << "Testing SignalStorage..." << std::endl;

    SignalStorage storage;

    std::shared_ptr<Signal> ds = std::make_shared<DigitalSignal>("ds", std::vector<float>{1, 2, 3});
    std::shared_ptr<Signal> as = std::make_shared<AnalogSignal>("as", std::vector<float>{1.0, 2.0, 3.0}, std::vector<float>{0.0, 1.0, 2.0});
    storage.addSignal(ds);
    storage.addSignal(as);

    // Проверка findLongestSignal
    auto longestSignal = storage.findLongestSignal();
    std::cout << "Longest signal ID: " << longestSignal->getId() << std::endl;

    // Проверка findMinMaxValues
    auto [minValue, maxValue] = storage.findMinMaxValues();
    std::cout << "Min value: " << minValue << ", Max value: " << maxValue << std::endl;
}

void testSignalOperation() {
    std::cout << "Testing SignalOperation..." << std::endl;

    std::shared_ptr<Signal> ds1 = std::make_shared<DigitalSignal>("ds1", std::vector<float>{1, 2, 3});
    std::shared_ptr<Signal> ds2 = std::make_shared<DigitalSignal>("ds2", std::vector<float>{4, 5});

    SignalOperation op("concatenate", [](const Signal& s1, const Signal& s2) {
        return s1.concatenate(s2);
    });

    auto result = op.execute(*ds1, *ds2);
    std::cout << "Concatenated signal ID: " << result->getId() << std::endl;
}

void testProcessingPipeline() {
    std::cout << "Testing ProcessingPipeline..." << std::endl;

    SignalStorage storage;

    storage.addSignal(std::make_shared<DigitalSignal>("ds1", std::vector<float>{1, 2, 3}));
    storage.addSignal(std::make_shared<DigitalSignal>("ds2", std::vector<float>{4, 5}));

    ProcessingPipeline pipeline(storage);

    pipeline.addOperation("concatenate", "ds1", "ds2", "result");
    pipeline.validate();
    pipeline.execute();
    std::cout << "Pipeline executed successfully!" << std::endl;
}

void testCopyConstructorAndAssignmentOperator() {
    std::cout << "Testing Copy Constructor and Assignment Operator..." << std::endl;

    std::vector<float> signalData = {1.0, 2.0, 3.0};
    DigitalSignal originalSignal("original", signalData);

    DigitalSignal copiedSignal(originalSignal);
    std::cout << "Copied Signal ID: " << copiedSignal.getId() << std::endl;
    std::cout << "Value at time 1 in copied signal: " << copiedSignal[1] << std::endl;

    DigitalSignal anotherSignal("another", {4.0, 5.0});

    anotherSignal = originalSignal;
    std::cout << "Another Signal ID after assignment: " << anotherSignal.getId() << std::endl;
    std::cout << "Value at time 2 in another signal after assignment: " << anotherSignal[2] << std::endl;
}

int main() {
    try {
        testDigitalSignal();
        testAnalogSignal();
        testSinusoidalSignal();
        testSignalStorage();
        testSignalOperation();
        testProcessingPipeline();
        testCopyConstructorAndAssignmentOperator();
    } catch (const std::exception& e) {
        std::cerr << "Test failed: " << e.what() << std::endl;
    }

    return 0;
}
