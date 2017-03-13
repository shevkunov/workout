#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
}

void MainWindow::paintEvent(QPaintEvent *event)
{
    (void) event;
    QPainter painter(this);

    QImage img(320, 240, QImage::Format_RGB32);
    for (int i = 0; i < img.width(); ++i)
        for (int j = 0; j < img.height(); ++j)
            img.setPixelColor(i, j, QColor(255, i * 255 / img.width() , j * 255 / img.height()));

    painter.drawImage(5, 5, img);
}

MainWindow::~MainWindow()
{
    delete ui;
}
