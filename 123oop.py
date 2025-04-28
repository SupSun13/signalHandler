import sys
sys.path.append("/Users/sergej/projects/cproj/SignalHandler/build/build")  # Путь к папке с библиотекой
import signal_lib

sin_signal = signal_lib.SinusoidalSignal("sin1", 5.0, 2.0, 0.0)
print(f"Amplitude: {sin_signal.getAmplitude()}")  # 5.0
print(f"Value at t=1.0: {sin_signal.getValueAt(1.0)}")  # ≈ 0.0

# Создание цифрового сигнала
digital_signal = signal_lib.DigitalSignal("dig1", [1.0, 2.0, 3.0])
print(f"Digital signal value at t=1.5: {digital_signal.getValueAt(1.5)}")

# Добавление сигналов в хранилище
storage = signal_lib.SignalStorage()
storage.addSignal(sin_signal)
storage.addSignal(digital_signal)

# Поиск сигнала по ID
found_signal = storage.findSignalById("sin1")
if found_signal:
    print(f"Found signal: {found_signal.getId()}")  # "sin1"

# Поиск минимального и максимального значений
min_max = storage.findMinMaxValues()
print(f"Min: {min_max[0]}, Max: {min_max[1]}")

# Создание операции конкатенации
def concatenate_signals(s1, s2):
    return s1.concatenate(s2)

operation = signal_lib.SignalOperation("concat", concatenate_signals)
result = operation.execute(sin_signal, digital_signal)
print(f"Concatenated signal ID: {result.getId()}")

# Использование пайплайна
pipeline = signal_lib.ProcessingPipeline(storage)
pipeline.addOperation("concat", "sin1", "dig1", "result")
pipeline.validate()
pipeline.execute()

result_signal = storage.findSignalById("result")
if result_signal:
    print(f"Pipeline result: {result_signal.getId()}")  # "result"