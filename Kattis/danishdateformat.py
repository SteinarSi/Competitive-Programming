from datetime import datetime

months = [
    "januar", "februar", "marts", "april", "maj", "juni",
    "juli", "august", "september", "oktober", "november", "december"
]

date = datetime.strptime(input(), '%m/%d/%Y')
print(f'{date.day}. {months[date.month-1]} {date.year}')
