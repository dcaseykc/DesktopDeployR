#' This is an example shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial
print('run ui')
source('./startup.R')
input_width = '400px'
input_width_wide = '640px'
header <- dashboardHeader(
  disable = T
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Starting Information',
             menuSubItem('Global Options', tabName = 'go'),
             menuSubItem('Orders & Tiers', tabName = 'ot'),
             menuSubItem('Hospital Data', tabName = 'hosp'),
             menuSubItem('Inventory', tabName = 'inv'),
             menuSubItem('Item Classifications', tabName = 'itemclass'),
             menuSubItem('Other inputs', tabName = 'other_inputs'),
             startExpanded = TRUE
             ),
    menuItem('Make Allocations',
             menuSubItem('Validate Inputs', tabName = 'valid'),
             menuSubItem('Allocate', tabName = 'allocate'),
             startExpanded = TRUE
             ),
    menuItem('Reports',
             menuSubItem('Add Allocation data', tabName = 'reports'),
             startExpanded = TRUE),
    menuItem('Diagnostics/Notes',
             menuSubItem('Session Info', tabName = 'sessionInfo'),
             startExpanded = TRUE)
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'go',
            #textOutput('wd'),
            h2("Global Options"),
            dateInput('cycle_date', label = 'Cycle Date', value = Sys.Date() - (wday(Sys.Date())+1) ),
            textInput('workdir', label = 'Base Working Directory', value = defaults[variable == 'base_output', value], width = input_width),
            actionButton('go_validate', 'Create/load folder structure'),
            textOutput('workfolder')),
    tabItem(tabName = 'ot',
            h2('Orders & Tiers'),
            fileInput('ot_t1', 'Tier 1 aggregated requests', width = input_width, placeholder = 'Internal Tier 1 - Weekly PPE 20200724.xlsx'),
            fileInput('ot_t2', "Tier 2+ aggregated requests", width = input_width, placeholder = 'Internal Tier 2 - Weekly PPE 20200724.xlsx'),
            textInput('ot_ver', 'Order & Tiers version', 1),
            textInput('ot_prev_ver', 'Order & Tiers previous version (optional)', NULL),
            fileInput('ot_prev_sum', 'Summary of the previous week (asum)', width = input_width, placeholder = "asum_all_724_v2.csv"),
            fileInput('ot_dump', 'WebEOC data dump', width = input_width, placeholder = 'export.xlsx'),
            fileInput('ot_add', 'Additional Orders (optional)', width = input_width, placeholder = 'order_additions.xlsx'),
            actionButton('ot_run', 'Create order and tier sheets'),
            textOutput('t_o')),
    tabItem(tabName = 'hosp',
            h2('Hospital Data'),
            fileInput('hosp_inv','Hospital PPE Inventory', width = input_width, placeholder = 'Supplies Data Detail.xlsx'),
            fileInput('hosp_covid', 'Hospital COVID counts', width = input_width, placeholder = 'COVID-19 Data Detail.xlsx'),
            actionButton('hosp_go', 'Create hospital datasheet'),
            textOutput('hosp_o')),
    tabItem(tabName = 'inv',
            h2('Inventory'),
            textInput('inv_new_ver', 'New Inventory Version', value = 1),
            fileInput('inv_new', 'New Inventory Dataset', width = input_width, placeholder = 'Inventory_Balance_29Jul2020_1200AM.xls'),
            fileInput('inv_old', 'Old Inventory Classifications', width = input_width, placeholder = 'inventory_724_1.csv'),
            actionButton('inv_go', 'Prepare inventory'),
            textOutput('inv_o')),
    tabItem(tabName = 'itemclass',
            h2('Item Classifications'),
            fileInput('ic_old', 'Existing Item Classifications', width = input_width, placeholder = 'item_classifications.csv'),
            textInput('ic_new', 'Order version'),
            actionButton('ic_go', 'Make new classifications sheet'),
            textOutput('ic_o')),
    tabItem(tabName = 'other_inputs',
            h2('Other Inputs'),
            p('Note: Open & Confirmed Cases file under development and requires manual addition of cases_last_2_weeks column and licenses for Supported Living and any missing LTCF values.'),
            fileInput('cases', 'Open & Confirmed Cases', width = input_width_wide, placeholder = 'PPE Distribution - Open and Confirmed Investigations.csv'),
            fileInput('replacements', 'Replacement Instructions', width = input_width, placeholder = 'replacements.xlsx'),
            fileInput('acrciq', 'ACRC/IQ Occupancy', width = input_width, placeholder = 'acrciq.xlsx'),
            fileInput('chgs', 'PPE Changes by type', width = input_width, placeholder = 'chgs.xlsx'),
            fileInput('noallocate', 'Do not allocate agency-item list', width = input_width, placeholder = 'donotallocate.xlsx'),
            #actionButton('oi_go', 'Update/Upload'),
            textOutput('oi_o')),
    tabItem(tabName = 'valid',
            h2('Validate Inputs'),
            h3('QA should be on updated after item classification file and tier workbook completed'),
            textInput('qa_ot_v', 'Orders & Tiers Version', 1),
            actionButton('qa_go', 'QA Orders!'),
            textOutput('qa_o')
            ),
    tabItem(tabName = 'allocate',
            h2('Run Allocations'),
            textInput('cycle_v', 'Cycle Version', 1),
            textInput('inv_v', 'Inventory Version',1),
            textInput('ot_v', 'Orders & Tiers Version', 1),
            textInput('runtiers', 'Tiers to run (semi-colon seperated list)', value = defaults[variable == 'runtiers', value]),
            textInput('sized', 'Item types to distribute by size (semi-colon seperated list)',
                      value = defaults[variable == 'sized_items', value], width = input_width_wide),
            textInput('ignore_me', 'Item types to not distribute (semi-colon seperated list)', value = defaults[variable == 'do_not_distribute', value]),
            textInput('n95except', 'Allowed N95 exceptions (semi-colon seperated list)',
                      value = defaults[variable == 'N95_exceptions', value], width = input_width_wide),
            numericInput('holdback_frac', 'Inventory Distribute %', 95, 0, 100),
            textInput('hosp_thresh', 'Hospital Days of Supply Threshold', Inf),
            textInput('cache_folder', 'Directory to drake cache', value = defaults[variable == 'cache_folder', value]),
            actionButton('make_allocs', "Run Allocations"),
            textOutput('alloc_response')
            ),
    tabItem(tabName = 'reports',
            h2('Add current cycle allocations to reporting files'),
            p('Note: placeholder'),
            textInput('reports_cycle_v', 'Cycle Version'),
            textInput('reports_ot_v', 'Orders & Tiers Version'),
            # actionButton('reports_go', 'Run reporting scripts!'),
            textOutput('reports_o')
            ),
    tabItem(tabName = 'extras',
            verbatimTextOutput('sesh_deets'))
  )
)

ui = dashboardPage(header, sidebar, body)

