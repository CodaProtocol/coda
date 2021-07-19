// Provides: caml_bytes_of_uint8array
// Requires: caml_create_bytes, caml_bytes_unsafe_set
var caml_bytes_of_uint8array = function(uint8array) {
    var length = uint8array.length;
    var ocaml_bytes = caml_create_bytes(length);
    for (var i = 0; i < length; i++) {
        // No need to convert here: OCaml Char.t is just an int under the hood.
        caml_bytes_unsafe_set(ocaml_bytes, i, uint8array[i]);
    }
    return ocaml_bytes;
};

// Provides: caml_bytes_to_uint8array
// Requires: caml_ml_bytes_length, caml_bytes_unsafe_get
var caml_bytes_to_uint8array = function(ocaml_bytes) {
    var length = caml_ml_bytes_length(ocaml_bytes);
    var bytes = new joo_global_object.Uint8Array(length);
    for (var i = 0; i < length; i++) {
        // No need to convert here: OCaml Char.t is just an int under the hood.
        bytes[i] = caml_bytes_unsafe_get(ocaml_bytes, i);
    }
    return bytes;
};

// Provides: caml_bigint_256_of_numeral
// Requires: plonk_wasm, caml_jsstring_of_string
var caml_bigint_256_of_numeral = function (s, len, base) {
    return plonk_wasm.caml_bigint_256_of_numeral(caml_jsstring_of_string(s), len, base);
};

// Provides: caml_bigint_256_of_decimal_string
// Requires: plonk_wasm, caml_jsstring_of_string
var caml_bigint_256_of_decimal_string = function (s) {
    return plonk_wasm.caml_bigint_256_of_decimal_string(caml_jsstring_of_string(s));
};

// Provides: caml_bigint_256_num_limbs
// Requires: plonk_wasm
var caml_bigint_256_num_limbs = plonk_wasm.caml_bigint_256_num_limbs

// Provides: caml_bigint_256_bytes_per_limb
// Requires: plonk_wasm
var caml_bigint_256_bytes_per_limb = plonk_wasm.caml_bigint_256_bytes_per_limb

// Provides: caml_bigint_256_div
// Requires: plonk_wasm
var caml_bigint_256_div = plonk_wasm.caml_bigint_256_div

// Provides: caml_bigint_256_compare
// Requires: plonk_wasm
var caml_bigint_256_compare = plonk_wasm.caml_bigint_256_compare

// Provides: caml_bigint_256_print
// Requires: plonk_wasm
var caml_bigint_256_print = plonk_wasm.caml_bigint_256_print

// Provides: caml_bigint_256_to_string
// Requires: plonk_wasm, caml_string_of_jsstring
var caml_bigint_256_to_string = function(x) {
    return caml_string_of_jsstring(plonk_wasm.caml_bigint_256_to_string(x));
};

// Provides: caml_bigint_256_test_bit
// Requires: plonk_wasm, caml_js_to_bool
var caml_bigint_256_test_bit = function(x, i) {
    return caml_js_to_bool(plonk_wasm.caml_bigint_256_test_bit(x, i));
};

// Provides: caml_bigint_256_to_bytes
// Requires: plonk_wasm, caml_bytes_of_uint8array
var caml_bigint_256_to_bytes = function(x) {
    return caml_bytes_of_uint8array(plonk_wasm.caml_bigint_256_to_bytes(x));
};

// Provides: caml_bigint_256_of_bytes
// Requires: plonk_wasm, caml_bytes_to_uint8array
var caml_bigint_256_of_bytes = function(ocaml_bytes) {
    return plonk_wasm.caml_bigint_256_of_bytes(caml_bytes_to_uint8array(ocaml_bytes));
};

// Provides: caml_bigint_256_deep_copy
// Requires: plonk_wasm
var caml_bigint_256_deep_copy = plonk_wasm.caml_bigint_256_deep_copy





// Provides: caml_pasta_fp_copy
var caml_pasta_fp_copy = function(x, y) {
    for (var i = 0, l = x.length; i < l; i++) {
        x[i] = y[i];
    }
};

// Provides: caml_pasta_fp_option
var caml_pasta_fp_option = function(x) {
    // We encode 'none' in WASM as a biginteger formed from a series of
    // max-size u64s, which gets split into u8s. This value is never returned
    // as a valid field element, since it is larger than the field's modulus.
    for (var i = 0, l = x.length; i < l; i++) {
        if (x[i] != 255) {
            return [0, x]; // Some(x)
        }
    }
    return 0;
};

// Provides: caml_pasta_fp_size_in_bits
// Requires: plonk_wasm
var caml_pasta_fp_size_in_bits = plonk_wasm.caml_pasta_fp_size_in_bits

// Provides: caml_pasta_fp_size
// Requires: plonk_wasm
var caml_pasta_fp_size = plonk_wasm.caml_pasta_fp_size

// Provides: caml_pasta_fp_add
// Requires: plonk_wasm
var caml_pasta_fp_add = plonk_wasm.caml_pasta_fp_add

// Provides: caml_pasta_fp_sub
// Requires: plonk_wasm
var caml_pasta_fp_sub = plonk_wasm.caml_pasta_fp_sub

// Provides: caml_pasta_fp_negate
// Requires: plonk_wasm
var caml_pasta_fp_negate = plonk_wasm.caml_pasta_fp_negate

// Provides: caml_pasta_fp_mul
// Requires: plonk_wasm
var caml_pasta_fp_mul = plonk_wasm.caml_pasta_fp_mul

// Provides: caml_pasta_fp_div
// Requires: plonk_wasm
var caml_pasta_fp_div = plonk_wasm.caml_pasta_fp_div

// Provides: caml_pasta_fp_inv
// Requires: plonk_wasm, caml_pasta_fp_option
var caml_pasta_fp_inv = function(x) {
    caml_pasta_fp_option(plonk_wasm.caml_pasta_fp_inv(x));
};

// Provides: caml_pasta_fp_square
// Requires: plonk_wasm
var caml_pasta_fp_square = plonk_wasm.caml_pasta_fp_square

// Provides: caml_pasta_fp_is_square
// Requires: plonk_wasm, caml_js_to_bool
var caml_pasta_fp_is_square = function(x) {
    return caml_js_to_bool(plonk_wasm.caml_pasta_fp_is_square(x));
};

// Provides: caml_pasta_fp_sqrt
// Requires: plonk_wasm, caml_pasta_fp_option
var caml_pasta_fp_sqrt = function(x) {
    return caml_pasta_fp_option(plonk_wasm.caml_pasta_fp_sqrt(x));
};

// Provides: caml_pasta_fp_of_int
// Requires: plonk_wasm
var caml_pasta_fp_of_int = plonk_wasm.caml_pasta_fp_of_int

// Provides: caml_pasta_fp_to_string
// Requires: plonk_wasm, caml_string_of_jsstring
var caml_pasta_fp_to_string = function(x) {
    return caml_string_of_jsstring(plonk_wasm.caml_pasta_fp_to_string(x));
};

// Provides: caml_pasta_fp_of_string
// Requires: plonk_wasm, caml_jsstring_of_string
var caml_pasta_fp_of_string = function (x) {
    return plonk_wasm.caml_pasta_fp_of_string(caml_jsstring_of_string(x));
};

// Provides: caml_pasta_fp_print
// Requires: plonk_wasm
var caml_pasta_fp_print = plonk_wasm.caml_pasta_fp_print

// Provides: caml_pasta_fp_mut_add
// Requires: caml_pasta_fp_copy, caml_pasta_fp_add
var caml_pasta_fp_mut_add = function(x, y) {
    caml_pasta_fp_copy(x, caml_pasta_fp_add(x, y));
};

// Provides: caml_pasta_fp_mut_sub
// Requires: caml_pasta_fp_copy, caml_pasta_fp_sub
var caml_pasta_fp_mut_sub = function(x, y) {
    caml_pasta_fp_copy(x, caml_pasta_fp_sub(x, y));
};

// Provides: caml_pasta_fp_mut_mul
// Requires: caml_pasta_fp_copy, caml_pasta_fp_mul
var caml_pasta_fp_mut_mul = function(x, y) {
    caml_pasta_fp_copy(x, caml_pasta_fp_mul(x, y));
};

// Provides: caml_pasta_fp_mut_square
// Requires: caml_pasta_fp_copy, caml_pasta_fp_square
var caml_pasta_fp_mut_square = function(x) {
    caml_pasta_fp_copy(x, caml_pasta_fp_square(x));
};

// Provides: caml_pasta_fp_compare
// Requires: plonk_wasm
var caml_pasta_fp_compare = plonk_wasm.caml_pasta_fp_compare

// Provides: caml_pasta_fp_equal
// Requires: plonk_wasm
var caml_pasta_fp_equal = plonk_wasm.caml_pasta_fp_equal

// Provides: caml_pasta_fp_random
// Requires: plonk_wasm
var caml_pasta_fp_random = plonk_wasm.caml_pasta_fp_random

// Provides: caml_pasta_fp_rng
// Requires: plonk_wasm
var caml_pasta_fp_rng = plonk_wasm.caml_pasta_fp_rng

// Provides: caml_pasta_fp_to_bigint
// Requires: plonk_wasm
var caml_pasta_fp_to_bigint = plonk_wasm.caml_pasta_fp_to_bigint

// Provides: caml_pasta_fp_of_bigint
// Requires: plonk_wasm
var caml_pasta_fp_of_bigint = plonk_wasm.caml_pasta_fp_of_bigint

// Provides: caml_pasta_fp_two_adic_root_of_unity
// Requires: plonk_wasm
var caml_pasta_fp_two_adic_root_of_unity = plonk_wasm.caml_pasta_fp_two_adic_root_of_unity

// Provides: caml_pasta_fp_domain_generator
// Requires: plonk_wasm
var caml_pasta_fp_domain_generator = plonk_wasm.caml_pasta_fp_domain_generator

// Provides: caml_pasta_fp_to_bytes
// Requires: plonk_wasm, caml_bytes_of_uint8array
var caml_pasta_fp_to_bytes = function(x) {
    var res = plonk_wasm.caml_pasta_fp_to_bytes(x);
    return caml_bytes_of_uint8array(plonk_wasm.caml_pasta_fp_to_bytes(x));
};

// Provides: caml_pasta_fp_of_bytes
// Requires: plonk_wasm, caml_bytes_to_uint8array
var caml_pasta_fp_of_bytes = function(ocaml_bytes) {
    return plonk_wasm.caml_pasta_fp_of_bytes(caml_bytes_to_uint8array(ocaml_bytes));
};

// Provides: caml_pasta_fp_deep_copy
// Requires: plonk_wasm
var caml_pasta_fp_deep_copy = plonk_wasm.caml_pasta_fp_deep_copy





// Provides: caml_pasta_fq_copy
var caml_pasta_fq_copy = function(x, y) {
    for (var i = 0, l = x.length; i < l; i++) {
        x[i] = y[i];
    }
};

// Provides: caml_pasta_fq_option
var caml_pasta_fq_option = function(x) {
    // We encode 'none' in WASM as a biginteger formed from a series of
    // max-size u64s, which gets split into u8s. This value is never returned
    // as a valid field element, since it is larger than the field's modulus.
    for (var i = 0, l = x.length; i < l; i++) {
        if (x[i] != 255) {
            return [0, x]; // Some(x)
        }
    }
    return 0;
};

// Provides: caml_pasta_fq_size_in_bits
// Requires: plonk_wasm
var caml_pasta_fq_size_in_bits = plonk_wasm.caml_pasta_fq_size_in_bits

// Provides: caml_pasta_fq_size
// Requires: plonk_wasm
var caml_pasta_fq_size = plonk_wasm.caml_pasta_fq_size

// Provides: caml_pasta_fq_add
// Requires: plonk_wasm
var caml_pasta_fq_add = plonk_wasm.caml_pasta_fq_add

// Provides: caml_pasta_fq_sub
// Requires: plonk_wasm
var caml_pasta_fq_sub = plonk_wasm.caml_pasta_fq_sub

// Provides: caml_pasta_fq_negate
// Requires: plonk_wasm
var caml_pasta_fq_negate = plonk_wasm.caml_pasta_fq_negate

// Provides: caml_pasta_fq_mul
// Requires: plonk_wasm
var caml_pasta_fq_mul = plonk_wasm.caml_pasta_fq_mul

// Provides: caml_pasta_fq_div
// Requires: plonk_wasm
var caml_pasta_fq_div = plonk_wasm.caml_pasta_fq_div

// Provides: caml_pasta_fq_inv
// Requires: plonk_wasm, caml_pasta_fq_option
var caml_pasta_fq_inv = function(x) {
    caml_pasta_fq_option(plonk_wasm.caml_pasta_fq_inv(x));
};

// Provides: caml_pasta_fq_square
// Requires: plonk_wasm
var caml_pasta_fq_square = plonk_wasm.caml_pasta_fq_square

// Provides: caml_pasta_fq_is_square
// Requires: plonk_wasm, caml_js_to_bool
var caml_pasta_fq_is_square = function(x) {
    return caml_js_to_bool(plonk_wasm.caml_pasta_fq_is_square(x));
};

// Provides: caml_pasta_fq_sqrt
// Requires: plonk_wasm, caml_pasta_fq_option
var caml_pasta_fq_sqrt = function(x) {
    return caml_pasta_fq_option(plonk_wasm.caml_pasta_fq_sqrt(x));
};

// Provides: caml_pasta_fq_of_int
// Requires: plonk_wasm
var caml_pasta_fq_of_int = plonk_wasm.caml_pasta_fq_of_int

// Provides: caml_pasta_fq_to_string
// Requires: plonk_wasm, caml_string_of_jsstring
var caml_pasta_fq_to_string = function(x) {
    return caml_string_of_jsstring(plonk_wasm.caml_pasta_fq_to_string(x));
};

// Provides: caml_pasta_fq_of_string
// Requires: plonk_wasm, caml_jsstring_of_string
var caml_pasta_fq_of_string = function (x) {
    return plonk_wasm.caml_pasta_fq_of_string(caml_jsstring_of_string(x));
};

// Provides: caml_pasta_fq_print
// Requires: plonk_wasm
var caml_pasta_fq_print = plonk_wasm.caml_pasta_fq_print

// Provides: caml_pasta_fq_mut_add
// Requires: caml_pasta_fq_copy, caml_pasta_fq_add
var caml_pasta_fq_mut_add = function(x, y) {
    caml_pasta_fq_copy(x, caml_pasta_fq_add(x, y));
};

// Provides: caml_pasta_fq_mut_sub
// Requires: caml_pasta_fq_copy, caml_pasta_fq_sub
var caml_pasta_fq_mut_sub = function(x, y) {
    caml_pasta_fq_copy(x, caml_pasta_fq_sub(x, y));
};

// Provides: caml_pasta_fq_mut_mul
// Requires: caml_pasta_fq_copy, caml_pasta_fq_mul
var caml_pasta_fq_mut_mul = function(x, y) {
    caml_pasta_fq_copy(x, caml_pasta_fq_mul(x, y));
};

// Provides: caml_pasta_fq_mut_square
// Requires: caml_pasta_fq_copy, caml_pasta_fq_square
var caml_pasta_fq_mut_square = function(x) {
    caml_pasta_fq_copy(x, caml_pasta_fq_square(x));
};

// Provides: caml_pasta_fq_compare
// Requires: plonk_wasm
var caml_pasta_fq_compare = plonk_wasm.caml_pasta_fq_compare

// Provides: caml_pasta_fq_equal
// Requires: plonk_wasm
var caml_pasta_fq_equal = plonk_wasm.caml_pasta_fq_equal

// Provides: caml_pasta_fq_random
// Requires: plonk_wasm
var caml_pasta_fq_random = plonk_wasm.caml_pasta_fq_random

// Provides: caml_pasta_fq_rng
// Requires: plonk_wasm
var caml_pasta_fq_rng = plonk_wasm.caml_pasta_fq_rng

// Provides: caml_pasta_fq_to_bigint
// Requires: plonk_wasm
var caml_pasta_fq_to_bigint = plonk_wasm.caml_pasta_fq_to_bigint

// Provides: caml_pasta_fq_of_bigint
// Requires: plonk_wasm
var caml_pasta_fq_of_bigint = plonk_wasm.caml_pasta_fq_of_bigint

// Provides: caml_pasta_fq_two_adic_root_of_unity
// Requires: plonk_wasm
var caml_pasta_fq_two_adic_root_of_unity = plonk_wasm.caml_pasta_fq_two_adic_root_of_unity

// Provides: caml_pasta_fq_domain_generator
// Requires: plonk_wasm
var caml_pasta_fq_domain_generator = plonk_wasm.caml_pasta_fq_domain_generator

// Provides: caml_pasta_fq_to_bytes
// Requires: plonk_wasm, caml_bytes_of_uint8array
var caml_pasta_fq_to_bytes = function(x) {
    var res = plonk_wasm.caml_pasta_fq_to_bytes(x);
    return caml_bytes_of_uint8array(plonk_wasm.caml_pasta_fq_to_bytes(x));
};

// Provides: caml_pasta_fq_of_bytes
// Requires: plonk_wasm, caml_bytes_to_uint8array
var caml_pasta_fq_of_bytes = function(ocaml_bytes) {
    return plonk_wasm.caml_pasta_fq_of_bytes(caml_bytes_to_uint8array(ocaml_bytes));
};

// Provides: caml_pasta_fq_deep_copy
// Requires: plonk_wasm
var caml_pasta_fq_deep_copy = plonk_wasm.caml_pasta_fq_deep_copy





// Provides: caml_pasta_fp_vector_create
var caml_pasta_fp_vector_create = function() {
    return [];
};

// Provides: caml_pasta_fp_vector_length
var caml_pasta_fp_vector_length = function (v) {
    return v.length;
};

// Provides: caml_pasta_fp_vector_emplace_back
var caml_pasta_fp_vector_emplace_back = function (v, x) {
    v.push(x);
}

// Provides: caml_pasta_fp_vector_get
var caml_pasta_fp_vector_get = function (v, i) {
    return v[i];
}





// Provides: caml_pasta_fq_vector_create
var caml_pasta_fq_vector_create = function() {
    return [];
};

// Provides: caml_pasta_fq_vector_length
var caml_pasta_fq_vector_length = function (v) {
    return v.length;
};

// Provides: caml_pasta_fq_vector_emplace_back
var caml_pasta_fq_vector_emplace_back = function (v, x) {
    v.push(x);
}

// Provides: caml_pasta_fq_vector_get
var caml_pasta_fq_vector_get = function (v, i) {
    return v[i];
}





// Provides: free_finalization_registry
var free_finalization_registry =
    new joo_global_object.FinalizationRegistry(function (instance_representative) {
        instance_representative.free();
    });

// Provides: free_on_finalize
// Requires: free_finalization_registry
var free_on_finalize = function (x) {
    // This is an unfortunate hack: we're creating a second instance of the
    // class to be able to call free on it. We can't pass the value itself,
    // since the registry holds a strong reference to the representative value.
    //
    // However, the class is only really a wrapper around a pointer, with a
    // reference to the class' prototype as its __prototype__.
    //
    // It might seem cleaner to call the destructor here on the pointer
    // directly, but unfortunately the destructor name is some mangled internal
    // string generated by wasm_bindgen. For now, this is the best,
    // least-brittle way to free once the original class instance gets collected.
    var instance_representative = x.constructor.__wrap(x.ptr)
    free_finalization_registry.register(x, instance_representative);
};





// Provides: rust_affine_to_caml_affine
var rust_affine_to_caml_affine = function(pt) {
    var infinity = pt.infinity;
    if (infinity) {
        pt.free();
        return 0;
    } else {
        var x = pt.x;
        var y = pt.y;
        pt.free();
        return [0, [0, x, y]];
    }
};

// Provides: rust_affine_of_caml_affine
// Requires: free_on_finalize
var rust_affine_of_caml_affine = function(pt, mk_new) {
    var res = mk_new();
    if (pt === 0) {
        res.infinity = true;
    } else {
        // Layout is [0, [0, x, y]]
        // First 0 is the tag (it's the 0th constructor that takes arguments)
        // Second 0 is the block marker for the anonymous tuple arguments
        res.x = pt[1][1];
        res.y = pt[1][2];
    }
    free_on_finalize(res);
    return res;
};





// Provides: caml_pasta_pallas_one
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_one = function() {
    var res = plonk_wasm.caml_pasta_pallas_one();
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_add
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_add = function(x, y) {
    var res = plonk_wasm.caml_pasta_pallas_add(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_sub
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_sub = function(x, y) {
    var res = plonk_wasm.caml_pasta_pallas_sub(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_negate
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_negate = function(x) {
    var res = plonk_wasm.caml_pasta_pallas_negate(x);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_double
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_double = function(x) {
    var res = plonk_wasm.caml_pasta_pallas_double(x);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_scale
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_scale = function(x, y) {
    var res = plonk_wasm.caml_pasta_pallas_scale(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_random
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_random = function() {
    var res = plonk_wasm.caml_pasta_pallas_random();
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_rng
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_rng = function(i) {
    var res = plonk_wasm.caml_pasta_pallas_rng(i);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_to_affine
// Requires: plonk_wasm, rust_affine_to_caml_affine
var caml_pasta_pallas_to_affine = function(pt) {
    var res = plonk_wasm.caml_pasta_pallas_to_affine(pt);
    return rust_affine_to_caml_affine(res);
};

// Provides: caml_pasta_pallas_of_affine
// Requires: plonk_wasm, rust_affine_of_caml_affine, free_on_finalize
var caml_pasta_pallas_of_affine = function(pt) {
    var res = plonk_wasm.caml_pasta_pallas_of_affine(rust_affine_of_caml_affine(pt, plonk_wasm.caml_pasta_pallas_affine_one));
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_of_affine_coordinates
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_pallas_of_affine_coordinates = function(x, y) {
    var res = plonk_wasm.caml_pasta_pallas_of_affine_coordinates(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_pallas_endo_base
// Requires: plonk_wasm
var caml_pasta_pallas_endo_base = plonk_wasm.caml_pasta_pallas_endo_base;

// Provides: caml_pasta_pallas_endo_scalar
// Requires: plonk_wasm
var caml_pasta_pallas_endo_scalar = plonk_wasm.caml_pasta_pallas_endo_scalar;

// Provides: caml_pasta_pallas_affine_deep_copy
// Requires: plonk_wasm, rust_affine_of_caml_affine, rust_affine_to_caml_affine
var caml_pasta_pallas_affine_deep_copy = function(pt) {
    return rust_affine_to_caml_affine(plonk_wasm.caml_pasta_pallas_affine_deep_copy(rust_affine_of_caml_affine(pt, plonk_wasm.caml_pasta_pallas_affine_one)));
};





// Provides: caml_pasta_vesta_one
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_one = function() {
    var res = plonk_wasm.caml_pasta_vesta_one();
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_add
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_add = function(x, y) {
    var res = plonk_wasm.caml_pasta_vesta_add(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_sub
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_sub = function(x, y) {
    var res = plonk_wasm.caml_pasta_vesta_sub(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_negate
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_negate = function(x) {
    var res = plonk_wasm.caml_pasta_vesta_negate(x);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_double
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_double = function(x) {
    var res = plonk_wasm.caml_pasta_vesta_double(x);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_scale
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_scale = function(x, y) {
    var res = plonk_wasm.caml_pasta_vesta_scale(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_random
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_random = function() {
    var res = plonk_wasm.caml_pasta_vesta_random();
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_rng
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_rng = function(i) {
    var res = plonk_wasm.caml_pasta_vesta_rng(i);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_to_affine
// Requires: plonk_wasm, rust_affine_to_caml_affine
var caml_pasta_vesta_to_affine = function(pt) {
    var res = plonk_wasm.caml_pasta_vesta_to_affine(pt);
    return rust_affine_to_caml_affine(res);
};

// Provides: caml_pasta_vesta_of_affine
// Requires: plonk_wasm, rust_affine_of_caml_affine, free_on_finalize
var caml_pasta_vesta_of_affine = function(pt) {
    var res = plonk_wasm.caml_pasta_vesta_of_affine(rust_affine_of_caml_affine(pt, plonk_wasm.caml_pasta_vesta_affine_one));
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_of_affine_coordinates
// Requires: plonk_wasm, free_on_finalize
var caml_pasta_vesta_of_affine_coordinates = function(x, y) {
    var res = plonk_wasm.caml_pasta_vesta_of_affine_coordinates(x, y);
    free_on_finalize(res);
    return res;
};

// Provides: caml_pasta_vesta_endo_base
// Requires: plonk_wasm
var caml_pasta_vesta_endo_base = plonk_wasm.caml_pasta_vesta_endo_base;

// Provides: caml_pasta_vesta_endo_scalar
// Requires: plonk_wasm
var caml_pasta_vesta_endo_scalar = plonk_wasm.caml_pasta_vesta_endo_scalar;

// Provides: caml_pasta_vesta_affine_deep_copy
// Requires: plonk_wasm, rust_affine_of_caml_affine, rust_affine_to_caml_affine
var caml_pasta_vesta_affine_deep_copy = function(pt) {
    return rust_affine_to_caml_affine(plonk_wasm.caml_pasta_vesta_affine_deep_copy(rust_affine_of_caml_affine(pt, plonk_wasm.caml_pasta_vesta_affine_one)));
};
