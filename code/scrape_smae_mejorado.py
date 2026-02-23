"""
Script MEJORADO para extraer información nutricional del SMAE
Basado en la estructura real de la página
"""

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
import pandas as pd
import time
import re

BASE_URL = "https://www.sistemadigitaldealimentos.org"

GRUPOS = {
    "frutas": "Frutas",
    "verduras": "Verduras", 
    "cereales": "Cereales",
    "leguminosas": "Leguminosas",
    "origen_animal": "Origen Animal",
    "lacteos": "Lácteos",
    "azucar": "Azúcar",
    "grasas": "Grasas"
}

def setup_driver():
    """Configura Chrome en modo headless"""
    options = webdriver.ChromeOptions()
    options.add_argument('--headless')
    options.add_argument('--no-sandbox')
    options.add_argument('--disable-dev-shm-usage')
    options.add_argument('--window-size=1920,1080')
    
    driver = webdriver.Chrome(options=options)
    return driver

def cerrar_popup(driver):
    """Cierra el popup de registro si aparece"""
    try:
        # Intentar múltiples selectores para el botón de cerrar
        selectores = [
            "//button[contains(@class, 'close')]",
            "//button[@data-dismiss='modal']",
            "//button[contains(text(), 'Close')]",
            "//*[@class='close']"
        ]
        
        for selector in selectores:
            try:
                button = WebDriverWait(driver, 3).until(
                    EC.element_to_be_clickable((By.XPATH, selector))
                )
                button.click()
                time.sleep(0.5)
                return True
            except:
                continue
                
        # Si no funciona, presionar ESC
        from selenium.webdriver.common.keys import Keys
        driver.find_element(By.TAG_NAME, 'body').send_keys(Keys.ESCAPE)
        
    except:
        pass
    
    return False

def extraer_alimentos_de_grupo(driver, grupo_key, grupo_nombre):
    """Extrae la lista de alimentos de un grupo"""
    url = f"{BASE_URL}/equivalentes/grupo/{grupo_key}"
    print(f"\n{'='*70}")
    print(f"📂 {grupo_nombre}")
    print(f"{'='*70}")
    
    driver.get(url)
    time.sleep(2)
    cerrar_popup(driver)
    
    alimentos = []
    
    try:
        # Buscar enlaces de alimentos (que terminan en /al)
        links = driver.find_elements(By.XPATH, "//a[contains(@href, '/equivalentes/alimentos/') and contains(@href, '/al')]")
        
        for link in links:
            try:
                texto = link.text.strip()
                url_alimento = link.get_attribute('href')
                
                if texto and '/al' in url_alimento:
                    # Separar nombre y porción
                    partes = [p.strip() for p in texto.split('|')]
                    nombre = partes[0]
                    porcion = ' | '.join(partes[1:]) if len(partes) > 1 else ""
                    
                    # Extraer ID
                    match = re.search(r'/(\d+)/al$', url_alimento)
                    id_alimento = match.group(1) if match else ""
                    
                    alimentos.append({
                        'id': id_alimento,
                        'nombre': nombre,
                        'porcion': porcion,
                        'grupo': grupo_nombre,
                        'url': url_alimento
                    })
            except:
                continue
        
        # Eliminar duplicados
        alimentos_unicos = []
        ids_vistos = set()
        for alimento in alimentos:
            if alimento['id'] not in ids_vistos:
                ids_vistos.add(alimento['id'])
                alimentos_unicos.append(alimento)
        
        print(f"✓ {len(alimentos_unicos)} alimentos encontrados")
        return alimentos_unicos
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return []

def extraer_info_nutricional_completa(driver, url_alimento, nombre):
    """
    Extrae TODA la información nutricional basándose en la estructura real:
    - Calorías (de la gráfica)
    - Macronutrientes % (Hdec, Prot, Líp)
    - Tabla de nutrientes detallada
    """
    try:
        driver.get(url_alimento)
        time.sleep(2)
        cerrar_popup(driver)
        
        info = {
            'nombre_completo': '',
            'peso_porcion': '',
            'calorias': None,
            'hdec_pct': None,
            'prot_pct': None,
            'lip_pct': None,
        }
        
        # 1. Extraer nombre completo y peso de la porción
        try:
            # El título completo aparece en un h5 o similar
            titulo_elem = driver.find_element(By.XPATH, "//*[contains(text(), 'g') and contains(@class, 'text-center')]")
            titulo = titulo_elem.text.strip()
            
            # Extraer peso en gramos
            peso_match = re.search(r'(\d+)\s*g', titulo)
            if peso_match:
                info['peso_porcion'] = peso_match.group(1)
            
            info['nombre_completo'] = titulo
        except:
            pass
        
        # 2. Extraer calorías del texto "XX CAL"
        try:
            cal_elem = driver.find_element(By.XPATH, "//*[contains(text(), 'CAL')]")
            cal_text = cal_elem.text
            cal_match = re.search(r'(\d+)\s*CAL', cal_text, re.I)
            if cal_match:
                info['calorias'] = int(cal_match.group(1))
        except:
            pass
        
        # 3. Extraer porcentajes de macronutrientes de la leyenda de la gráfica
        try:
            # Buscar los elementos con "Hdec XX%", "Prot XX%", "Líp XX%"
            elementos_pct = driver.find_elements(By.XPATH, "//*[contains(text(), '%')]")
            
            for elem in elementos_pct:
                texto = elem.text
                
                hdec = re.search(r'Hdec\s*(\d+)%', texto, re.I)
                prot = re.search(r'Prot\s*(\d+)%', texto, re.I)
                lip = re.search(r'L[íi]p\s*(\d+)%', texto, re.I)
                
                if hdec:
                    info['hdec_pct'] = int(hdec.group(1))
                if prot:
                    info['prot_pct'] = int(prot.group(1))
                if lip:
                    info['lip_pct'] = int(lip.group(1))
        except:
            pass
        
        # 4. Extraer la TABLA completa de nutrientes
        # La estructura es: <table> con filas que tienen Nutrimento | Cantidad
        try:
            tabla = driver.find_element(By.TAG_NAME, "table")
            filas = tabla.find_elements(By.TAG_NAME, "tr")
            
            for fila in filas:
                try:
                    celdas = fila.find_elements(By.TAG_NAME, "td")
                    if len(celdas) >= 2:
                        nutriente = celdas[0].text.strip()
                        cantidad = celdas[1].text.strip()
                        
                        if nutriente and cantidad:
                            # Normalizar nombre de nutriente para usar como columna
                            # Quitar caracteres especiales y normalizar
                            col_name = nutriente.lower()
                            col_name = col_name.replace('á', 'a').replace('é', 'e').replace('í', 'i')
                            col_name = col_name.replace('ó', 'o').replace('ú', 'u').replace('ñ', 'n')
                            col_name = re.sub(r'[^\w\s]', '', col_name)
                            col_name = col_name.replace(' ', '_')
                            col_name = col_name.replace('.', '')
                            
                            # Guardar con el nombre del nutriente
                            info[col_name] = cantidad
                except:
                    continue
        except:
            pass
        
        return info
        
    except Exception as e:
        print(f"    ✗ Error al extraer {nombre}: {e}")
        return {}

def scrape_smae_completo(grupos_a_procesar=None):
    """
    Función principal para hacer scraping completo del SMAE
    
    Args:
        grupos_a_procesar: Lista de grupos a procesar (None = todos)
                          Ejemplo: ['origen_animal', 'lacteos']
    """
    print("="*70)
    print("🚀 INICIANDO SCRAPING COMPLETO DEL SMAE")
    print("="*70)
    
    driver = setup_driver()
    
    # Determinar qué grupos procesar
    if grupos_a_procesar:
        grupos = {k: v for k, v in GRUPOS.items() if k in grupos_a_procesar}
    else:
        grupos = GRUPOS
    
    todos_los_alimentos = []
    
    try:
        # PASO 1: Extraer lista de alimentos
        print("\n📋 PASO 1: Extrayendo lista de alimentos")
        print("="*70)
        
        for grupo_key, grupo_nombre in grupos.items():
            alimentos = extraer_alimentos_de_grupo(driver, grupo_key, grupo_nombre)
            todos_los_alimentos.extend(alimentos)
            time.sleep(1)
        
        print(f"\n✓ Total de alimentos encontrados: {len(todos_los_alimentos)}")
        
        # PASO 2: Extraer información nutricional detallada
        print("\n📊 PASO 2: Extrayendo información nutricional")
        print("="*70)
        
        resultados = []
        total = len(todos_los_alimentos)
        
        for i, alimento in enumerate(todos_los_alimentos, 1):
            nombre_corto = alimento['nombre'][:40]
            print(f"[{i}/{total}] {nombre_corto}...", end=' ', flush=True)
            
            info_nutricional = extraer_info_nutricional_completa(
                driver, 
                alimento['url'],
                alimento['nombre']
            )
            
            # Combinar datos básicos con información nutricional
            resultado = {
                'id': alimento['id'],
                'nombre': alimento['nombre'],
                'porcion': alimento['porcion'],
                'grupo': alimento['grupo'],
                'url': alimento['url'],
                **info_nutricional
            }
            
            resultados.append(resultado)
            
            # Mostrar si se obtuvo información
            if info_nutricional.get('calorias'):
                print(f"✓ ({info_nutricional.get('calorias')} kcal)")
            else:
                print("✗")
            
            # Pausas para no sobrecargar
            if i % 10 == 0:
                time.sleep(2)
            else:
                time.sleep(0.5)
        
        return resultados
        
    finally:
        driver.quit()

def guardar_resultados(resultados, nombre_base='smae_completo'):
    """Guarda los resultados en múltiples formatos"""
    if not resultados:
        print("\n❌ No hay datos para guardar")
        return None
    
    df = pd.DataFrame(resultados)
    
    # Reordenar columnas: info básica primero, luego nutricional
    cols_basicas = ['id', 'nombre', 'porcion', 'grupo', 'peso_porcion', 
                    'calorias', 'hdec_pct', 'prot_pct', 'lip_pct']
    
    cols_existentes = [c for c in cols_basicas if c in df.columns]
    cols_nutrientes = [c for c in df.columns if c not in cols_existentes]
    
    df = df[cols_existentes + cols_nutrientes]
    
    # Guardar en diferentes formatos
    csv_path = f'/mnt/user-data/outputs/{nombre_base}.csv'
    df.to_csv(csv_path, index=False, encoding='utf-8-sig')
    print(f"\n💾 CSV guardado: {csv_path}")
    
    excel_path = f'/mnt/user-data/outputs/{nombre_base}.xlsx'
    df.to_excel(excel_path, index=False)
    print(f"💾 Excel guardado: {excel_path}")
    
    # Resumen
    print(f"\n{'='*70}")
    print("📊 RESUMEN DE DATOS EXTRAÍDOS")
    print(f"{'='*70}")
    print(f"Total de alimentos: {len(df)}")
    print(f"\nAlimentos por grupo:")
    print(df['grupo'].value_counts().to_string())
    
    print(f"\n🔬 Nutrientes extraídos:")
    nutrientes = [c for c in df.columns if c not in ['id', 'nombre', 'porcion', 'grupo', 'url', 'nombre_completo']]
    print(f"Total: {len(nutrientes)} columnas")
    for i in range(0, len(nutrientes), 4):
        print("  " + ", ".join(nutrientes[i:i+4]))
    
    # Mostrar cuántos tienen datos completos
    print(f"\n✅ Alimentos con calorías: {df['calorias'].notna().sum()}")
    print(f"✅ Alimentos con macronutrientes: {df['hdec_pct'].notna().sum()}")
    
    return df

if __name__ == "__main__":
    # OPCIÓN 1: Procesar solo un grupo para probar (rápido)
    print("\n🧪 MODO DE PRUEBA: Solo procesando 'Origen Animal'")
    print("(Cambia grupos_a_procesar=None para procesar todos)\n")
    
    resultados = scrape_smae_completo(grupos_a_procesar=['origen_animal'])
    
    if resultados:
        df = guardar_resultados(resultados, nombre_base='smae_origen_animal_completo')
        
        # Mostrar muestra de los primeros 5 alimentos
        print(f"\n🔍 MUESTRA DE DATOS (primeros 5 alimentos):")
        print("="*70)
        cols_mostrar = ['nombre', 'porcion', 'calorias', 'hdec_pct', 'prot_pct', 'lip_pct']
        print(df[cols_mostrar].head().to_string(index=False))
    
    print("\n" + "="*70)
    print("✅ PROCESO COMPLETADO")
    print("="*70)
    print("\n💡 Para procesar TODOS los grupos, cambia:")
    print("   resultados = scrape_smae_completo(grupos_a_procesar=None)")

