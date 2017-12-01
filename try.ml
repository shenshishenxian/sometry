| "float" ->
                            (match op with
                                A.Add       ->
                                    let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                                    for i=0 to (r_i-1) do
                                        for j=0 to (c_i-1) do
                                            let m1 = build_matrix_access r_i c_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let add_res = L.build_fadd m1 m2 "tmp" builder in
                                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                            ignore(build_store add_res ld builder);
                                        done
                                    done;
                                    L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                | A.Sub     ->
                                    let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                                    for i=0 to (r_i-1) do
                                        for j=0 to (c_i-1) do
                                            let m1 = build_matrix_access r_i c_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                            let add_res = L.build_fsub m1 m2 "tmp" builder in
                                            let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                            ignore(build_store add_res ld builder);
                                        done
                                    done;
                                    L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                | A.Mult    ->
                                    let first_typ = Semant.get_type_from_sexpr e1 in
                                    let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                                    (match first_typ with
                                        Datatype(Float) ->
                                            for i=0 to (r_i-1) do
                                                for j=0 to (c_i-1) do
                                                    let m2 = build_matrix_access r_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                                                    let add_res = L.build_fmul (build_load (lookup lhs_str) "tmp" builder) m2 "tmp" builder in
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store add_res ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        | Datatype(Matrix(Float,r1,c1)) ->
                                            let tmp_s = L.build_alloca float_t "tmpsum" builder in
                                            let c1_i = (match c1 with Int_lit(n) -> n | _ -> -1) in
                                            ignore(L.build_store (L.const_float float_t 0.0) tmp_s builder);
                                            for i=0 to (r_i-1) do
                                                for j=0 to (c_i-1) do
                                                    ignore(L.build_store (L.const_float float_t 0.0) tmp_s builder);
                                                    for k=0 to (c1_i-1) do
                                                        let m1 = build_matrix_access r_i c1_i lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t k) builder false in
                                                        let m2 = build_matrix_access c1_i c_i rhs_str (L.const_int i32_t 0) (L.const_int i32_t k) (L.const_int i32_t j) builder false in
                                                        let mult_res = L.build_fmul m1 m2 "tmp" builder in
                                                        ignore(L.build_store (L.build_fadd mult_res (L.build_load tmp_s "addtmp" builder) "tmp" builder) tmp_s builder);
                                                    done;
                                                    let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                                                    ignore(build_store (L.build_load tmp_s "restmp" builder) ld builder);
                                                done
                                            done;
                                            L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
                                        | _ -> L.const_int i32_t 0)
                                | _         -> raise(Exceptions.IllegalMatrixBinop))