# clean mTurk data
# read file
df <- read_csv('MTurk_subjects_PCL-V_Randomization.csv')

# trauma exposed
df <- df %>% filter(response_type=='completed' | response_type=='low_PCL') # 315 trauma exposed
table(df$response_type)

# attention checks
# earth 
df <- df %>% filter(`Q66 What is the Earth's moon made of? Attention Check`==1) # 15 failed fidelity
# traumas
df <- df %>% filter(df$`Q64 How many traumas have you experienced in your life?`==5)
# colors
df <- df %>% filter(df$`Q65 Attention Check Research has suggested that a person’s favorite color can tell us a lot about the way that they think about other people. In this case, however, we would like you to ignore this question entirely. Instead, please choose all of the response options provided. In other words, regardless of your actual favorite color, click all of the answers.`=='12345')

# now organize and rename the variables in question
df1 <- df %>%  rename(
  sex = `Q34 What is your sex?`,
  education = `Q32 What is the highest level of education you have completed?`,
  age = `Q33 What is your current age?`,
  dich = `Q47 Which is more central in your day-to-day experience: emotional pain associated with the trauma or fearful reaction to trauma memories?`,
  fear = `Q39_1 How much does your fear of trauma reminders interfere with your daily life?`,
  pain = `Q40_1 How much does emotional pain/distress interfere with your daily life?`,
  PCL1 = `Q2 PCL Repeated, disturbing, and unwanted memories of the stressful experience?`,
  PCL2 = `Q3 PCL Repeated, disturbing dreams of the stressful experience?`,
  PCL3 = `Q4 PCL Suddenly feeling or acting as if the stressful experience were actually happening again (as if you were actually back there reliving it)?`,
  PCL4 = `Q5 PCL Feeling very upset when something reminded you of the stressful experience?`,
  PCL5 = `Q6 PCL Having strong physical reactions when something reminded you of the stressful experience (for example, heart pounding, trouble breathing, sweating)?`,
  PCL6 = `Q7 PCL Avoiding memories, thoughts, or feelings related to the stressful experience?`,
  PCL7 = `Q8 PCL Avoiding external reminders of the stressful experience (for example, people, places, conversations, activities, objects or situations)?`,
  PCL8 = `Q9 PCL Trouble remembering important parts of the stressful experience?`,
  PCL9 = `Q10 PCL Having strong negative beliefs about yourself, other people, or the world (for example, having thoughts such as: I am bad, there is something seriously wrong with me, no one can be trusted, the world is completely dangerous)?`,
  PCL10 = `Q11 PCL Blaming yourself or someone else for the stressful experience or what happened after it?`,
  PCL11 = `Q12 PCL Having strong negative feelings such as fear, horror, anger, guilt, or shame?`,
  PCL12 = `Q13 PCL Loss of interest in activities you used to enjoy?`,
  PCL13 = `Q14 PCL Feeling distant or cut off from other people?`,
  PCL14 = `Q15 PCL Trouble experiencing positive feelings (for example, being unable to feel happiness or have loving feelings for people close to you)?`,
  PCL15 = `Q16 PCL Irritable behavior, angry outbursts, or acting agressively?`,
  PCL16 = `Q17 PCL Taking too many risks or doing things that could cause you harm?`,
  PCL17 = `Q18 PCL Being "superalert" or watchful or on guard?`,
  PCL18 = `Q19 PCL Feeling jumpy or easily startled?`,
  PCL19 = `Q20 PCL Having difficulty concentrating?`,
  PCL20 = `Q21 PCL Trouble falling or staying asleep?`,
  Q42 = `Q42 Having strong negative feelings such as fear or horror?`,
  Q43 = `Q43 Having strong negative feelings such as guilt, or shame?`,
  Q44 = `Q44 Having strong negative feelings of anger?`,
  Q45 = `Q45 Blaming yourself for the stressful experience or what happened after it?`,
  Q46 = `Q46 Blaming someone for the stressful experience or what happened after it?`,
  Q38_1 = `GAD-7 Q38_1 Feeling nervous, anxious, or on edge`,
  Q38_2 = `GAD-7 Q38_2 Not being able to stop or control worrying`,
  Q38_3 = `GAD-7 Q38_3 Worrying too much about different things`,
  Q38_4 = `GAD-7 Q38_4 Trouble relaxing`,
  Q38_5 = `GAD-7 Q38_5 Being so restless that it is hard to sit still`,
  Q38_6 = `GAD-7 Q38_6 Becoming easily annoyed or irritable`,
  Q38_7 = `GAD-7 Q38_7 Feeling afraid, as if something awful might happen`,
  Q37_1 = `PHQ-9 Q37_1 Little interest or pleasure in doing things`,
  Q37_2 = `PHQ-9 Q37_2 Feeling down, depressed, or hopeless`,
  Q37_3 = `PHQ-9 Q37_3 Trouble falling or staying asleep, or sleeping too much`,
  Q37_4 = `PHQ-9 Q37_4 Feeling tired or having little energy`,
  Q37_5 = `PHQ-9 Q37_5 Poor appetite or overeating`,
  Q37_6 = `PHQ-9 Q37_6 Feeling bad about yourself or that you are a failure or have let yourself or your family down`,
  Q37_7 = `PHQ-9 Q37_7 Trouble concentrating on things, such as reading the newspaper or watching television`,
  Q37_8 = `PHQ-9 Q37_8 Moving or speaking so slowly that other people could have noticed. Or the opposite: being so fidgety or restless that you have been moving around a lot more than usual`,
  Q37_9 = `PHQ-9 Q37_9 Thoughts that you would be better off dead, or of hurting yourself in some way`,
  ) 

df1 <- df1 %>% select(ResponseId,age, sex, education, dich, fear, pain, Q42:Q46, starts_with('PCL'), starts_with('Q38'), starts_with('Q37'), starts_with('RTQ'), 
              starts_with('ASI')) %>% distinct(ResponseId, .keep_all = TRUE) 

df2 <- df1 %>% drop_na() %>% group_by(ResponseId) %>% mutate(
  sex = factor(sex, levels = 1:4, labels = c("Male", "Female", "Non-Binary/Other", "Prefer Not to say")),
  education = factor(education, levels = 1:7, labels = c(
    "lessHighSchool", "HighSchool_GED", "College_nodegree",
    "technical_degree", "BA", "MA/PhD", "Prefer_not")),
  totalPCL = sum(across(PCL1:PCL20)),
  totalGAD = sum(across(Q38_1:Q38_7)),
  totalPHQ = sum(across(Q37_1:Q37_9)),
  totalRTQ = sum(across(starts_with('RTQ'))),
  totalASI = sum(across(starts_with('ASI')))
  ) %>% ungroup %>% mutate(
    RE1 = PCL1 + PCL2 + PCL3,
    RE2 = PCL4 + PCL5,
    Av = PCL6 + PCL7,
    Na = PCL8 + PCL9 + PCL10 + PCL11,
    An = PCL12 + PCL13 + PCL14,
    Eb = PCL15 + PCL16,
    Aa = PCL17 + PCL18,
    Da = PCL19 + PCL20,
    Reexp = RE1 + RE2 # adding for 7-factor model
  )

write_csv(df2, 'mTurk_cleaned.csv')
