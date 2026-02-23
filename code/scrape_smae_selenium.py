"""
Script mejorado para extraer información del SMAE usando Selenium
Maneja popups y JavaScript dinámico
"""

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException
import pandas as pd
import time
import re

BASE_URL = "https://www.sistemadigitaldealimentos.org"

GRUPOS = [
    "frutas", "verduras", "cereales", "leguminosas",
    "origen_animal", "lacteos", "azucar", "grasas"
]

def setup_driver():
    """Configura el driver de Chrome en modo headless"""
    options = webdriver.ChromeOptions()
    options.add_argument('--headless')
    options.add_argument('--no-sandbox')
    options.add_argument('--disable-dev-shm-usage')
    options.add_argument('--disable-gpu')
    options.add_argument('user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36')
    
    driver = webdriver.Chrome(options=options)
    return driver

def cerrar_popup_si_aparece(driver):
    """Intenta cerrar el popup de registro si aparece"""
    try:
        # Buscar botón de cerrar (X o Close)
        close_buttons = [
            (By.CLASS_NAME, "close"),
            (By.XPATH, "//button[contains(text(), 'Close')]"),
            (By.XPATH, "//button[contains(@class, 'close')]"),
            (By.CSS_SELECTOR, "button.btn-close")
        ]
        
        for by, selector in close_buttons:
            try:
                button = WebDriverWait(driver, 2).until(
                    EC.element_to_be_clickable((by, selector))
                )
                button.click()
                print("  ✓ Popup cerrado")
                time.sleep(0.5)
                return True
            except:
                continue
        
        # Si no se encuentra botón, intentar hacer clic fuera del modal
        try:
            driver.find_element(By.TAG_NAME, "body").click()
        except:
            pass
            
    except Exception as e:
        pass  # No hay popup o no se pudo cerrar
    
    return False

def extraer_alimentos_grupo(driver, grupo):
    """Extrae lista de alimentos de un grupo"""
    url = f"{BASE_URL}/equivalentes/grupo/{grupo}"
    print(f"\n{'='*60}")
    print(f"Procesando: {grupo.upper()}")
    print(f"{'='*60}")
    
    driver.get(url)
    time.sleep(2)
    cerrar_popup_si_aparece(driver)
    
    alimentos = []
    
    try:
        # Buscar todos los enlaces de alimentos
        links = driver.find_elements(By.CSS_SELECTOR, "a[href*='/equivalentes/alimentos/']")
        
        for link in links:
            try:
                texto = link.text.strip()
                url_alimento = link.get_attribute('href')
                
                # Filtrar solo enlaces de alimentos individuales (no recetas)
                if '/al' in url_alimento and texto:
                    # Extraer nombre y porción
                    partes = texto.split('|')
                    nombre = partes[0].strip()
                    porcion = partes[-1].strip() if len(partes) > 1 else ""
                    
                    alimentos.append({
                        'nombre': nombre,
                        'porcion': porcion,
                        'url': url_alimento,
                        'grupo': grupo
                    })
            except:
                continue
        
        # Eliminar duplicados
        alimentos_unicos = []
        urls_vistas = set()
        for alimento in alimentos:
            if alimento['url'] not in urls_vistas:
                urls_vistas.add(alimento['url'])
                alimentos_unicos.append(alimento)
        
        print(f"✓ Encontrados {len(alimentos_unicos)} alimentos")
        return alimentos_unicos
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return []

def extraer_info_nutricional(driver, url_alimento):
    """Extrae información nutricional de un alimento"""
    try:
        driver.get(url_alimento)
        time.sleep(1)
        cerrar_popup_si_aparece(driver)
        
        info = {}
        
        # Buscar calorías
        try:
            calorias_elem = driver.find_element(By.XPATH, "//*[contains(text(), 'CAL')]")
            cal_text = calorias_elem.text
            cal_match = re.search(r'(\d+)\s*CAL', cal_text, re.I)
            if cal_match:
                info['energia_kcal'] = int(cal_match.group(1))
        except:
            pass
        
        # Buscar macronutrientes (Hdec, Prot, Líp)
        try:
            # Buscar elementos con texto que contenga porcentajes
            elementos = driver.find_elements(By.XPATH, "//*[contains(text(), '%')]")
            for elem in elementos:
                texto = elem.text
                
                hdec = re.search(r'Hdec\s*(\d+)%', texto, re.I)
                prot = re.search(r'Prot\s*(\d+)%', texto, re.I)
                lip = re.search(r'L[íi]p\s*(\d+)%', texto, re.I)
                
                if hdec:
                    info['carbohidratos_pct'] = int(hdec.group(1))
                if prot:
                    info['proteinas_pct'] = int(prot.group(1))
                if lip:
                    info['lipidos_pct'] = int(lip.group(1))
        except:
            pass
        
        # Buscar tabla de nutrientes si existe
        try:
            tabla = driver.find_element(By.TAG_NAME, "table")
            filas = tabla.find_elements(By.TAG_NAME, "tr")
            
            for fila in filas:
                celdas = fila.find_elements(By.TAG_NAME, "td")
                if len(celdas) == 2:
                    nutriente = celdas[0].text.strip()
                    valor = celdas[1].text.strip()
                    
                    # Limpiar nombre de nutriente para usarlo como columna
                    col_name = re.sub(r'[^\w\s]', '', nutriente.lower())
                    col_name = col_name.replace(' ', '_')
                    
                    info[col_name] = valor
        except:
            pass
        
        return info
        
    except Exception as e:
        return {}

def scrape_completo():
    """Función principal"""
    print("="*60)
    print("INICIANDO SCRAPING DEL SMAE")
    print("="*60)
    
    driver = setup_driver()
    todos_los_alimentos = []
    
    try:
        # Paso 1: Extraer lista de alimentos
        for grupo in GRUPOS:
            alimentos = extraer_alimentos_grupo(driver, grupo)
            todos_los_alimentos.extend(alimentos)
            time.sleep(1)
        
        print(f"\n{'='*60}")
        print(f"TOTAL: {len(todos_los_alimentos)} alimentos")
        print(f"{'='*60}")
        
        # Paso 2: Extraer información nutricional
        print("\nExtrayendo información nutricional...\n")
        
        resultados = []
        for i, alimento in enumerate(todos_los_alimentos, 1):
            nombre_corto = alimento['nombre'][:45]
            print(f"[{i}/{len(todos_los_alimentos)}] {nombre_corto}...", end=' ')
            
            info = extraer_info_nutricional(driver, alimento['url'])
            
            resultado = {
                'nombre': alimento['nombre'],
                'porcion': alimento['porcion'],
                'grupo': alimento['grupo'],
                'url': alimento['url'],
                **info
            }
            
            resultados.append(resultado)
            print(f"✓" if info else "✗")
            
            if i % 5 == 0:
                time.sleep(1)
        
        return resultados
        
    finally:
        driver.quit()

def guardar_datos(resultados):
    """Guarda los resultados"""
    if not resultados:
        print("No hay datos para guardar")
        return
    
    df = pd.DataFrame(resultados)
    
    # Guardar CSV
    csv_path = '/mnt/user-data/outputs/smae_alimentos.csv'
    df.to_csv(csv_path, index=False, encoding='utf-8-sig')
    print(f"\n✓ CSV guardado: {csv_path}")
    
    # Guardar Excel
    excel_path = '/mnt/user-data/outputs/smae_alimentos.xlsx'
    df.to_excel(excel_path, index=False)
    print(f"✓ Excel guardado: {excel_path}")
    
    # Resumen
    print(f"\n{'='*60}")
    print("RESUMEN")
    print(f"{'='*60}")
    print(f"Total alimentos: {len(df)}")
    print(f"\nPor grupo:")
    print(df['grupo'].value_counts().to_string())
    print(f"\nColumnas: {list(df.columns)}")
    
    return df

if __name__ == "__main__":
    resultados = scrape_completo()
    df = guardar_datos(resultados)
    print("\n✓ COMPLETADO")
