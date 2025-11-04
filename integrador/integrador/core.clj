(ns integrador.core
  (:gen-class))

(defn cadastrar-alunos [alunos]
  (loop [alunos alunos]
    (println "Nome do aluno (deixe em branco para sair):")
    (let [nome (read-line)]
      (if (empty? nome)
        alunos
        (do
          (println "Nota:")
          (let [nota (Double/parseDouble (read-line))]
            (recur (conj alunos {:nome nome :nota nota}))))))))

(defn relatorio-notas [alunos]
  (if (empty? alunos)
    (println "\n=== RELATÓRIO DE NOTAS ===\nNenhum aluno cadastrado.")
    (let [alunos-status (map #(assoc % :status (if (>= (:nota %) 6.0) "Aprovado" "Reprovado")) alunos)
          aprovados (filter #(= (:status %) "Aprovado") alunos-status)
          media (/ (reduce + (map :nota alunos)) (count alunos))]
      (println "\n=== RELATÓRIO DE NOTAS ===")
      (doseq [a aprovados]
        (println (:nome a) "-" (:nota a) "-" (:status a)))
      (println "Média geral:" (format "%.2f" media))
      alunos-status)))

(defn estatisticas-gerais [alunos]
  (if (empty? alunos)
    (println "\n=== ESTATÍSTICAS GERAIS ===\nNenhum aluno cadastrado.")
    (let [total (count alunos)
          aprovados (count (filter #(>= (:nota %) 6.0) alunos))
          reprovados (- total aprovados)
          maior (apply max (map :nota alunos))
          menor (apply min (map :nota alunos))
          media (/ (reduce + (map :nota alunos)) total)]
      (println "\n=== ESTATÍSTICAS GERAIS ===")
      (println "Total de alunos:" total)
      (println "Aprovados:" aprovados)
      (println "Reprovados:" reprovados)
      (println "Maior nota:" maior)
      (println "Menor nota:" menor)
      (println "Média geral:" (format "%.2f" media)))))

(defn buscar-aluno [alunos]
  (println "Digite o nome do aluno:")
  (let [nome (read-line)
        aluno (first (filter #(= (:nome %) nome) alunos))]
    (if aluno
      (let [status (if (>= (:nota aluno) 6.0) "Aprovado" "Reprovado")]
        (println (:nome aluno) "-" (:nota aluno) "-" status))
      (println "Aluno não encontrado."))))

(defn -main []
  (loop [alunos []]
    (println "\n=== MENU PRINCIPAL ===")
    (println "1 - Cadastrar Alunos")
    (println "2 - Relatório de Notas")
    (println "3 - Estatísticas Gerais")
    (println "4 - Buscar Aluno")
    (println "0 - Sair")
    (let [opcao (read-line)]
      (cond
        (= opcao "1") (recur (cadastrar-alunos alunos))
        (= opcao "2") (do (relatorio-notas alunos) (recur alunos))
        (= opcao "3") (do (estatisticas-gerais alunos) (recur alunos))
        (= opcao "4") (do (buscar-aluno alunos) (recur alunos))
        (= opcao "0") (println "Encerrando...")
        :else (do (println "Opção inválida!") (recur alunos))))))
