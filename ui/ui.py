import sys
import numpy as np
import matplotlib
matplotlib.use('QtAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas
from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
    QPushButton, QComboBox, QLineEdit, QLabel, QListWidget, QFormLayout
)
sys.path.append("/Users/sergej/projects/cproj/SignalHandler/build/build")
import signal_lib
import os
from pyswip import Prolog


class SignalPlotter(QWidget):
    def __init__(self):
        super().__init__()
        self.figure, self.ax = plt.subplots()
        self.canvas = FigureCanvas(self.figure)
        layout = QVBoxLayout()
        layout.addWidget(self.canvas)
        self.setLayout(layout)

    def plot_signals(self, signals, time_range=(0, 10), num_points=1000):
        self.ax.clear()
        t = np.linspace(time_range[0], time_range[1], num_points)
        has_labels = False

        for signal in signals:
            try:
                y = [signal.getValueAt(float(x)) for x in t]
                line, = self.ax.plot(t, y, label=signal.getId())
                label = line.get_label()
                if isinstance(label, str) and len(label) > 0 and label[0] != '_':
                    has_labels = True
            except Exception as e:
                print(f"Ошибка отрисовки: {str(e)}")

        if has_labels:
            self.ax.legend()
        self.ax.grid(True)
        self.canvas.draw()


class SignalCreator(QWidget):
    def __init__(self, storage, update_callback):
        super().__init__()
        self.storage = storage
        self.update_callback = update_callback

        self.signal_type = QComboBox()
        self.signal_type.addItems(["Sinusoidal", "Digital", "Analog"])
        self.signal_type.currentTextChanged.connect(self.update_parameters)

        self.init_ui()

    def init_ui(self):
        layout = QFormLayout()

        layout.addRow("Тип сигнала:", self.signal_type)

        self.id_input = QLineEdit("signal1")
        self.param1 = QLineEdit()
        self.param2 = QLineEdit()
        self.param3 = QLineEdit()

        self.create_btn = QPushButton("Создать сигнал")
        self.create_btn.clicked.connect(self.create_signal)

        layout.addRow("Тип сигнала:", self.signal_type)
        layout.addRow("ID:", self.id_input)
        layout.addRow("Param1:", self.param1)
        layout.addRow("Param2:", self.param2)
        layout.addRow("Param3:", self.param3)
        layout.addRow(self.create_btn)

        self.setLayout(layout)
        self.update_parameters()

    def update_parameters(self):
        signal_type = self.signal_type.currentText()

        self.param1.setVisible(True)
        self.param2.setVisible(True)
        self.param3.setVisible(True)

        if signal_type == "Sinusoidal":
            self.param1.setPlaceholderText("Amplitude")
            self.param2.setPlaceholderText("Period")
            self.param3.setPlaceholderText("Phase")
        elif signal_type == "Digital":
            self.param1.setPlaceholderText("Durations (через запятую)")
            self.param2.setVisible(False)
            self.param3.setVisible(False)
        elif signal_type == "Analog":
            self.param1.setPlaceholderText("Values (через запятую)")
            self.param2.setPlaceholderText("Times (через запятую)")
            self.param3.setVisible(False)

    def create_signal(self):
        try:
            signal_id = self.id_input.text()
            if not signal_id:
                raise ValueError("Введите ID сигнала")

            if self.signal_type.currentText() == "Sinusoidal":
                params = [
                    float(self.param1.text()),
                    float(self.param2.text()),
                    float(self.param3.text())
                ]
                signal = signal_lib.SinusoidalSignal(signal_id, *params)

            elif self.signal_type.currentText() == "Digital":
                durations = list(map(float, self.param1.text().split(',')))
                signal = signal_lib.DigitalSignal(signal_id, durations)

            elif self.signal_type.currentText() == "Analog":
                values = list(map(float, self.param1.text().split(',')))
                times = list(map(float, self.param2.text().split(',')))
                signal = signal_lib.AnalogSignal(signal_id, values, times)

            self.storage.addSignal(signal)
            print(f"Создан сигнал: {signal_id}")
            self.update_callback()

        except Exception as e:
            print(f"Ошибка создания: {str(e)}")


class PrologAnalyzer:
    def __init__(self):
        self.prolog = Prolog()
        self._prepare_prolog()

    def _prepare_prolog(self):
        try:
            prolog_file = os.path.join(os.path.dirname(__file__), "signal_rules.pl")
            self.prolog.consult(prolog_file)
        except Exception as e:
            print(f"Ошибка инициализации Prolog: {str(e)}")

    def analyze(self, signals, query_type):
        try:
            self.prolog.retractall("signal(_,_,_,_,_)")

            for signal in signals:
                if isinstance(signal, signal_lib.SinusoidalSignal):
                    self.prolog.assertz(
                        f"signal('{signal.getId()}', sinusoidal, {signal.getAmplitude()}, {signal.getPeriod()}, {signal.getPhase()})"
                    )
                elif isinstance(signal, signal_lib.DigitalSignal):
                    self.prolog.assertz(
                        f"signal('{signal.getId()}', digital, {signal.getDurations()}, {signal.getHighLevel()}, _)"
                    )
                elif isinstance(signal, signal_lib.AnalogSignal):
                    self.prolog.assertz(
                        f"signal('{signal.getId()}', analog, {signal.getValues()}, {signal.getTimes()}, _)"
                    )

            if query_type == "longest_digital":
                result = list(self.prolog.query("longest_digital(Id)"))
            elif query_type == "max_value":
                result = list(self.prolog.query("max_value_signal(Id)"))

            return [res['Id'] for res in result] if result else []

        except Exception as e:
            print(f"Ошибка анализа: {str(e)}")
            return []

class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.storage = signal_lib.SignalStorage()
        self.prolog_analyzer = PrologAnalyzer()
        self.init_ui()

    def init_ui(self):
        self.setWindowTitle("Signal Visualizer")
        self.setGeometry(100, 100, 1200, 600)

        main_widget = QWidget()
        main_layout = QHBoxLayout()

        # Левая панель
        left_panel = QVBoxLayout()

        # Создание сигналов
        self.signal_creator = SignalCreator(self.storage, self.update_interface)
        left_panel.addWidget(self.signal_creator)

        # Список сигналов
        self.signal_list = QListWidget()
        left_panel.addWidget(QLabel("Доступные сигналы:"))
        left_panel.addWidget(self.signal_list)

        # Анализ
        analysis_layout = QVBoxLayout()
        self.analysis_combo = QComboBox()
        self.analysis_combo.addItems([
            "Самый длинный цифровой сигнал",
            "Сигнал с максимальным значением"
        ])
        self.analyze_btn = QPushButton("Анализировать")
        self.analyze_btn.clicked.connect(self.run_analysis)
        self.result_label = QLabel("Результат: ")

        analysis_layout.addWidget(QLabel("Анализ сигналов:"))
        analysis_layout.addWidget(self.analysis_combo)
        analysis_layout.addWidget(self.analyze_btn)
        analysis_layout.addWidget(self.result_label)
        left_panel.addLayout(analysis_layout)

        # Правая панель
        self.plotter = SignalPlotter()

        main_layout.addLayout(left_panel, 1)
        main_layout.addWidget(self.plotter, 2)
        main_widget.setLayout(main_layout)
        self.setCentralWidget(main_widget)

        self.update_interface()

    def update_interface(self):
        self.update_signal_list()
        signals = self.storage.getAllSignals()
        if signals:
            self.plotter.plot_signals(signals)
        else:
            self.plotter.ax.clear()
            self.plotter.ax.set_title("Нет данных для отображения")
            self.plotter.canvas.draw()

    def update_signal_list(self):
        self.signal_list.clear()
        try:
            signals = self.storage.getAllSignals()
            for signal in signals:
                self.signal_list.addItem(signal.getId())
        except Exception as e:
            print(f"Ошибка обновления списка: {str(e)}")

    def run_analysis(self):
        try:
            signals = self.storage.getAllSignals()
            if not signals:
                self.result_label.setText("Нет сигналов для анализа")
                return

            query_mapping = {
                "Самый длинный цифровой сигнал": "longest_digital",
                "Сигнал с максимальным значением": "max_value"
            }

            selected_text = self.analysis_combo.currentText()
            query_type = query_mapping.get(selected_text)

            if not query_type:
                raise ValueError(f"Неизвестный тип запроса: {selected_text}")

            result = self.prolog_analyzer.analyze(signals, query_type)

            if result:
                self.result_label.setText(f"Результат: {result[0]}")
            else:
                self.result_label.setText("Нет результатов")

        except Exception as e:
            self.result_label.setText(f"Ошибка: {str(e)}")


if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    sys.exit(app.exec())