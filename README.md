# KOR-Classes-VCS-Export
Экспорт КОР-класса для систем контроля версий

Утилита для выгрузки метаданных класса в файлы удобочитаемого вида, для последующей загрузки их в используемую на проекте систему контроля версий.

Файлы не пригодны импорта в КОР. Возможность использования для импорта не рассматривается.

## Установка
1. Скачать [последнюю версию](https://github.com/ParusPmo/KOR-Classes-VCS-Export/releases/latest);
2. Выполнить скрипт KOR-Classes-VCS-Export-Install.sql под владельцем схемы Парус-8;
3. В КОР для класса «DMSClasses» («Классы»):
  1. Добавить метод:
```toml
["Метод"]
    "Мнемокод" = "EXPORT_CLASS_FULL"
    "Тип метода" = "Процедура"
    "Доступность" = "Клиентский"
    "Пакет" = "UDO_PKG_DMSCLASS_DIFF_EXPORT"
    "Процедура/функция" = "EXPORT_CLASS_FULL"
    "Наименование" = "Выгрузка в файлы"
    ["Метод.Параметры"]
        [["Параметр"]]
            "Имя" = "A_CLASS_RN"
            "Наименование" = "A_CLASS_RN"
            "Позиция" = 1
            "Тип" = "Входной (in)"
            "Домен" = "TRN"
            "Тип привязки" = "Контекст"
            "Контекст" = "Идентификатор записи"
            "Обязательный для заполнения" = false
        [["Параметр"]]
            "Имя" = "A_FILEBUFF_IDENT"
            "Наименование" = "A_FILEBUFF_IDENT"
            "Позиция" = 2
            "Тип" = "Входной (in)"
            "Домен" = "TRN"
            "Тип привязки" = "Контекст"
            "Контекст" = "Идентификатор процесса"
            "Обязательный для заполнения" = false
```
  2. Добавить действие:
```toml
["Действие"]
    "Тип" = "Стандартный файловый экспорт"
    "Код" = "EXPORT_CLASS_FULL"
    "Наименование" = "Выгрузить в файлы"
    "Технология производства" = "Конструктор"
    "Позиция" = 1040
    "Реализующий метод" = "EXPORT_CLASS_FULL"
    "Обработка записей" = "Для одной текущей записи"
    "Завершение транзакции" = "После каждого вызова действия"
    "Обновление выборки" = "Не обновлять"
    "Показывать диалог при отсутствии визуализируемых параметров" = true
    "Отображать только при технологии производства «Конструктор»" = false
    "Безусловная доступность" = false
```
