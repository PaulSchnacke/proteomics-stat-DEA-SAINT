import sys
import os
from PyQt6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout, 
                             QHBoxLayout, QLabel, QPushButton, QFileDialog, 
                             QRadioButton, QButtonGroup, QLineEdit, QGroupBox, 
                             QMessageBox, QFrame)
from PyQt6.QtCore import Qt, QMimeData
from PyQt6.QtGui import QDragEnterEvent, QDropEvent, QFont, QPalette, QColor

from converter import diann_to_fragpipe_mimic

class DragDropLabel(QLabel):
    def __init__(self, parent_app):
        super().__init__("\nDrag & Drop your .tsv file here\n\n- OR -\n\nClick to Browse")
        self.parent_app = parent_app
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setStyleSheet("""
            QLabel {
                border: 2px dashed #aaa;
                border-radius: 10px;
                background-color: #f9f9f9;
                color: #555;
                font-size: 14px;
            }
            QLabel:hover {
                background-color: #f0f0f0;
                border-color: #777;
            }
        """)
        self.setAcceptDrops(True)
        self.setMinimumHeight(150)

    def mousePressEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self.parent_app.browse_file()

    def dragEnterEvent(self, event: QDragEnterEvent):
        if event.mimeData().hasUrls():
            event.accept()
        else:
            event.ignore()

    def dropEvent(self, event: QDropEvent):
        files = [u.toLocalFile() for u in event.mimeData().urls()]
        if files:
            self.parent_app.set_input_file(files[0])

class ConverterApp(QMainWindow):
    def __init__(self):
        super().__init__()

        self.setWindowTitle("DIANN to FragPipe Converter")
        self.setGeometry(100, 100, 700, 550)
        
        # Variables
        self.input_file_path = ""
        self.analysis_level = "protein"

        # Main Layout
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        main_layout = QVBoxLayout(central_widget)
        main_layout.setSpacing(20)
        main_layout.setContentsMargins(30, 30, 30, 30)

        # 1. Header
        header_layout = QVBoxLayout()
        title = QLabel("DIANN Output Converter")
        title.setFont(QFont("Arial", 20, QFont.Weight.Bold))
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        subtitle = QLabel("Prepare files for FragPipe-Analyst")
        subtitle.setFont(QFont("Arial", 12))
        subtitle.setAlignment(Qt.AlignmentFlag.AlignCenter)
        subtitle.setStyleSheet("color: #666;")
        
        header_layout.addWidget(title)
        header_layout.addWidget(subtitle)
        main_layout.addLayout(header_layout)

        # 2. File Selection (Drag & Drop)
        self.drop_label = DragDropLabel(self)
        main_layout.addWidget(self.drop_label)
        
        self.file_path_label = QLabel("No file selected")
        self.file_path_label.setStyleSheet("color: #333; font-style: italic; margin-top: 5px;")
        self.file_path_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        main_layout.addWidget(self.file_path_label)

        # 3. Settings Group
        settings_group = QGroupBox("Settings")
        settings_layout = QVBoxLayout()

        # Analysis Level
        level_layout = QHBoxLayout()
        level_layout.addWidget(QLabel("Analysis Level:"))
        
        self.rb_protein = QRadioButton("Protein (pg_matrix)")
        self.rb_peptide = QRadioButton("Peptide (pr_matrix)")
        self.rb_protein.setChecked(True)
        self.rb_protein.toggled.connect(self.update_level)
        
        self.level_group = QButtonGroup()
        self.level_group.addButton(self.rb_protein)
        self.level_group.addButton(self.rb_peptide)
        
        level_layout.addWidget(self.rb_protein)
        level_layout.addWidget(self.rb_peptide)
        level_layout.addStretch()
        settings_layout.addLayout(level_layout)

        # Output Prefix
        prefix_layout = QHBoxLayout()
        prefix_layout.addWidget(QLabel("Output Filename Prefix:"))
        self.prefix_input = QLineEdit()
        self.prefix_input.setPlaceholderText("e.g. experiment_1")
        prefix_layout.addWidget(self.prefix_input)
        settings_layout.addLayout(prefix_layout)

        settings_group.setLayout(settings_layout)
        main_layout.addWidget(settings_group)

        # 4. Action Button
        self.btn_convert = QPushButton("CONVERT FILES")
        self.btn_convert.setFont(QFont("Arial", 14, QFont.Weight.Bold))
        self.btn_convert.setStyleSheet("""
            QPushButton {
                background-color: #4CAF50; 
                color: white; 
                padding: 12px; 
                border-radius: 6px;
            }
            QPushButton:hover {
                background-color: #45a049;
            }
            QPushButton:pressed {
                background-color: #3d8b40;
            }
        """)
        self.btn_convert.setCursor(Qt.CursorShape.PointingHandCursor)
        self.btn_convert.clicked.connect(self.run_conversion)
        main_layout.addWidget(self.btn_convert)

        # 5. Status
        self.status_label = QLabel("Ready")
        self.status_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.status_label.setStyleSheet("color: #777; margin-top: 10px;")
        main_layout.addWidget(self.status_label)

    def browse_file(self):
        fname, _ = QFileDialog.getOpenFileName(self, 'Open TSV file', '.', "TSV Files (*.tsv);;All Files (*)")
        if fname:
            self.set_input_file(fname)

    def set_input_file(self, path):
        self.input_file_path = path
        self.file_path_label.setText(path)
        self.drop_label.setText("\nFile Selected!\n\nDrag another file to change")
        self.drop_label.setStyleSheet("""
            QLabel {
                border: 2px solid #4CAF50;
                border-radius: 10px;
                background-color: #e8f5e9;
                color: #2e7d32;
                font-size: 14px;
            }
        """)
        self.suggest_settings(path)

    def suggest_settings(self, path):
        filename = os.path.basename(path).lower()
        if "pr_matrix" in filename or "peptide" in filename:
            self.rb_peptide.setChecked(True)
        else:
            self.rb_protein.setChecked(True)
        
        base = os.path.splitext(filename)[0]
        base = base.replace(".pg_matrix", "").replace(".pr_matrix", "").replace("_report", "")
        self.prefix_input.setText(base)

    def update_level(self):
        if self.rb_protein.isChecked():
            self.analysis_level = "protein"
        else:
            self.analysis_level = "peptide"

    def run_conversion(self):
        if not self.input_file_path or not os.path.exists(self.input_file_path):
            QMessageBox.critical(self, "Error", "Please select a valid input file.")
            return

        prefix = self.prefix_input.text().strip()
        if not prefix:
            QMessageBox.warning(self, "Warning", "Please enter an output prefix.")
            return

        self.status_label.setText("Converting... Please wait.")
        self.btn_convert.setEnabled(False)
        QApplication.processEvents()

        try:
            input_dir = os.path.dirname(self.input_file_path)
            # Ensure safe path joining if prefix is just a name
            if os.path.isabs(prefix):
                full_prefix = prefix
            else:
                full_prefix = os.path.join(input_dir, prefix)
            
            mat_out, ann_out = diann_to_fragpipe_mimic(self.input_file_path, self.analysis_level, full_prefix)
            
            completion_msg = (f"Created files:\n\n"
                              f"{os.path.basename(mat_out)}\n"
                              f"{os.path.basename(ann_out)}\n\n"
                              f"Location: {os.path.dirname(mat_out)}")
            
            QMessageBox.information(self, "Success", completion_msg)
            self.status_label.setText("Conversion Complete")

        except Exception as e:
            QMessageBox.critical(self, "Error", f"An error occurred:\n{str(e)}")
            self.status_label.setText("Error during conversion")
            print(e)
        
        self.btn_convert.setEnabled(True)

if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = ConverterApp()
    window.show()
    sys.exit(app.exec())
